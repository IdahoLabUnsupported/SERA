/* ===========================================================> dcmlib.c
 *
 * Library of support routines for accessing dicom 3.0 files.
 *
 * Notes:
 *   Originally coded only for reading files - 8/96, GJH
 *
 * ======================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "libdcm.h"
#include "dicom_dd.h"

#define   MIN(a,b) (a < b) ? a : b


/*
 * Global control variables.
 *
 * vr_type_g indicates the type of encoding used for value representations.
 *    0 = implicit (the default)
 *    1 = explicit
 *
 * transfer_syntax_type_g is the transfer syntax derived from the
 * file.  It is used to set vr_type_g and to control some other
 * functionality.
 *    0 = Implicit VR Little Endian syntax (default)
 *    1 = Explicit VR Little Endian syntax (may indicate encapsulation)
 *    2 = Explicit VR Big Endian syntax
 *
 * There are numerous jpeg compression methods allowed. (18 at last count).
 * Only the following will be considered here at present.  These imply the
 * default data transfer syntax (I think).  Note that the jpeg compression
 * only applies to pixel data.
 *    0 = none
 *    1 = lossless (method 14)
 *    2 = lossy 8 bit (method 1)
 *    3 = lossy 12 bit (method 4)
 *
 * machine_type_g is the word organization.
 *    0 = big endian
 *    1 = little endian
 */

static int             initialized_g = 0;
struct
{
   file_type_T     file_type;
   transfer_type_T transfer_syntax_type;
   jpeg_type_T     jpeg_method;
   encoding_type_T vr_type;
   machine_type_T  machine_type;
} file_info_g;

struct
{
   int    px_rows;           /* image rows (0028, 0010) */
   int    px_cols;           /* image cols (0028, 0011) */
   int    px_samples;        /* samples per pixel (0028, 0002) */
   int    px_bits_alloc;     /* bits allocated per pixel (0028,0100) */
   int    px_bits_stored;    /* bits used for pixel rep (0028,0101) */
   int    px_high_bit;       /* high bit relative to bits alloc (0028,0102) */
   int    px_rep;            /* pixel rep (0028,0103) - 
                                0 = unsigned int
                                1 = 2's complement binary */
   int    px_necho;          /* number of echos */
   char   px_photointerp [32]; /* monochrome, rgb, ... (0028, 0004) */
   int    px_smallest;       /* smallest pixel (0028,0106) */
   int    px_largest;        /* largest pixel (0028,0107) */
} pixel_data_g;




int DcmSearchSequence (FILE *, unsigned short, unsigned short, 
                       dataval_T *, int);
int DcmHandleSQ (FILE *, unsigned int);
int ReadItem (FILE *, short *, short *, unsigned long *);

void ExtractByteString  (FILE *, VRTYPE_T, unsigned long, int, dataval_T *);
void ExtractValue (FILE *, VRTYPE_T, unsigned long, int, dataval_T *);
void ByteSwitch (char *, int);
int GetDDEntry (int, int);
void FlushField (FILE *, unsigned long);
VRTYPE_T ConvertVRT (char *);
void SetTransferSyntax (char   *);
void SetMachineType ();
int DcmSearchPart10 (FILE *, unsigned short, unsigned short, dataval_T *);
void ProcessGroupLengthItem (FILE *, short, VRTYPE_T, unsigned long);

int ExtractPixelData (FILE *, unsigned long, dataval_T *);
int ConvertRGBData (dataval_T *, unsigned long);
int ConvertGrayData (dataval_T *, unsigned long);
int ExtractAudioData (FILE *, unsigned long, dataval_T *);
int ExtractCurveData (FILE *, unsigned long, dataval_T *);



int DcmReadTag 
(
   FILE           *dcmfile, 
   void           *data,
   unsigned int   type,
   unsigned int   num,
   unsigned short group, 
   unsigned short elem
)
{
   dataval_T        dataval;
   int              nitem, ct, err;
   char             str [16], *cp1, *cp2;
   unsigned short   *sp1, *sp2;
   unsigned long    *lp1, *lp2;
   float            *fp1, *fp2;
   double           *dp1, *dp2;

   /*
    * Make sure we're initialized.
    */

   if (dcmfile == NULL)
      return (-1);

   if (initialized_g == 0)
      return (-1);

   /*
    * See if the item is legal.
    */

   if (GetDDEntry (group, elem) < 0)
      return (-1);

   /*
    * Initialize for the search. Rewind and for DICOM Part 10 files,
    * jump to byte 132.
    */

   rewind (dcmfile);
   if (file_info_g.file_type == DICOM_FILE)
   {
      fread (str, (size_t) 1, (size_t) 132, dcmfile);
   }


   err = DcmSearchSequence (dcmfile, group, elem, &dataval, 0);
   if (err < 0)
      return (-1);

   /*
    * Convert the data to the appropriate type for the caller.
    */

   switch (type)
   {
      case UNKNOWN:
      case CHAR_TYPE:
         if (dataval . dv_type == CHAR_TYPE) 
            memcpy (data, dataval . dv_value, dataval . dv_num);
         else
            err = -2;
         break;

      case INT_TYPE:
         if (dataval . dv_type != INT_TYPE)
            err = -2;
         else
         {
            nitem = MIN (dataval . dv_num, num);
            memcpy (data, dataval . dv_value, sizeof (long) * nitem);
         }
         break;

      case FLOAT_TYPE:
         if (dataval . dv_type != FLOAT_TYPE)
            err = -2;
         else
         {
            nitem = MIN (dataval . dv_num, num);
            memcpy (data, dataval . dv_value, sizeof (float) * nitem);
         }
         break;

      case DATAVAL_TYPE:
         data = &dataval;
         break;
   }

   free (dataval . dv_value);

   return (err);
}

int DcmInitSearch 
(
   FILE     *dcmfile
)
{
   char      str [16];

   /*
    * Rewind the file and then search for the group and element
    * tags.
    */

   rewind (dcmfile);

   /*
    * Initialize the control variables.
    */

   SetMachineType ();
   file_info_g.vr_type = IMPLICIT;   /* Assume implicit encoding */
   file_info_g.transfer_syntax_type = IMP_LITTLE_ENDIAN;
   file_info_g.jpeg_method = NOJPEG;

   pixel_data_g . px_rows = 0;
   pixel_data_g . px_cols = 0;
   pixel_data_g . px_bits_alloc = 16;
   pixel_data_g . px_samples = 1;
   pixel_data_g . px_bits_stored = 16;
   pixel_data_g . px_high_bit = 15;
   pixel_data_g . px_rep = 0;
   pixel_data_g . px_photointerp [0] = '\0';
   pixel_data_g . px_smallest = 0;
   pixel_data_g . px_largest = 65535;

   /*
    * Check to see what type of file this is.  A standard DICOM 3.0 format
    * or a DICOM 3.0 Part 10 encapsulated format file.  Search for the
    * data item and if flawed, return an error.
    */

   fread (str, (size_t) 1, (size_t) 4, dcmfile);
   rewind (dcmfile);
   if (strncmp (str, "DICM", 4) == 0)
      return (0);    /* Part 10 type file */
   else
      return (1);    /* regular Dicom 3.0 file */

   /*return 1;*/ /* removed by CLA 6-22-98, no chance of getting here */
}

char *DcmReadPixelData
(
   FILE           *dcmfile, 
   int            *nbyte,
   int            *planarity
)
{
   dataval_T     dataval;
   int           err, filetype;
   char          str [16];


   /*
    * Make sure we're initialized.
    */

   if (dcmfile == NULL)
      return (NULL);

   if (initialized_g == 0)
      return (NULL);

   /*
    * Initialize for the search. Rewind and for DICOM Part 10 files,
    * jump to byte 132.
    */

   rewind (dcmfile);
   if (file_info_g.file_type == DICOM_FILE)
   {
      fread (str, (size_t) 1, (size_t) 132, dcmfile);
   }

   err = DcmSearchSequence (dcmfile, 0x7FE0, 0x0010, &dataval, 0);
   if (err < 0)
      return (NULL);

   /*
    * The pixel data should be properly converted with regard to 
    * endian-ship and bit registry.  Return it with the number
    * of bytes per pixel and planarity.
    */

   if (pixel_data_g . px_samples == 3)
      *planarity = RGB_PHOTOINTERP;
   else
      if (pixel_data_g . px_samples == 1)
         *planarity = GRAY_PHOTOINTERP;
      else
         *planarity = UNKNOWN_PHOTOINTERP;

   if (pixel_data_g . px_bits_alloc == 16)
      *nbyte = 2;
   else
      *nbyte = 1;

   return ((char *) dataval . dv_value);
}

int DcmGetImageInfo 
(
   FILE       *dcmfile,
   long       *rows,
   long       *cols,
   long       *bytesperpixel,
   long       *necho
)
{
   long       tmp;

   /*
    * Since the fields are sorted by group and element number in the file,
    * fetching the largest group and element of interest will set all
    * of the others.  Test for a reasonable file at this point.
    */

   DcmReadTag (dcmfile, &tmp, INT_TYPE, 1,
            DCM_PIXEL_REP_GRP, DCM_PIXEL_REP_ELM);
   
   if (pixel_data_g . px_high_bit < 0)
   {
#ifdef DEBUG
      fprintf (stderr, "** Bits allocatedc not properly set\n");
#endif
      return (-1);
   }

   if (pixel_data_g . px_bits_stored <= 0)
   {
#ifdef DEBUG
      fprintf (stderr, "** Bits stored not properly set\n");
#endif
      return (-1);
   }
   
   if (pixel_data_g . px_high_bit < 0)
   {
#ifdef DEBUG
      fprintf (stderr, "** High  bit not properly set\n");
#endif
      return (-1);
   }


   /* 
    * Store and return.
    */

   
   *bytesperpixel = (int) (pixel_data_g . px_bits_alloc/ 8 + 0.9);
   *rows = pixel_data_g . px_rows;
   *cols = pixel_data_g . px_cols;

/*
   if (pixel_data_g . px_bits_stored >= 8)
      *bps = 16;
   else
      *bps = 8;
   
   if (strcmp (pixel_data_g . px_photointerp, "RGB") == 0)
      *photointerp = RGB_PHOTOINTERP;
   else
      if (strncmp (pixel_data_g . px_photointerp, "MONO", 4) == 0)
         *photointerp = GRAY_PHOTOINTERP;
      else
         *photointerp = UNKNOWN_PHOTOINTERP;
*/

   *necho = pixel_data_g . px_necho;

   return (0);
}


int DcmSearchSequence 
(
   FILE              *dcmfile,
   unsigned short    sgroup,
   unsigned short    selem,
   dataval_T         *dataval,
   int               depth
)
{
   unsigned short  group, element, len_defined;
   char            type [3], value [256];
   char            msg [64];
   unsigned long   length, seq_length;
   VRTYPE_T        vrtype;       
   int             entry, ct;
   DD_ENTRY_T      dd, *dd_entry;
   
   /*
    * Search a sequence of data items. Recursion is allowed. 
    * A depth of 0 indicates that this is the outside level, consisting
    * of the basic set of data items.  A depth > 0  indicates that
    * it is the result of an SQ entry in the file.
    */

   /*
    * If this is the outside level, there is no need to get the length.
    * Otherwise, the get length and set the flag to indicate if the 
    * length is explicit or implicit.
    */

   len_defined = FALSE;
   seq_length = 0;
   if (depth != 0)
   {
      fread (&seq_length, (size_t) 4, (size_t) 1, dcmfile);
      if (seq_length != 0xFFFFFFFF)
         len_defined = TRUE;
   }
   
   
   /*
    * Read through the file, comparing each data element field.  The file
    * pointer is passed back and forth to the routines, since data element
    * fields vary greatly in size.
    */

   do
   {
      entry = ReadItem (dcmfile, &group, &element, &length);
#ifdef DEBUG
         sprintf (msg, "Read group = %x element = %x\n", group, element);
         DcmAbort (msg);
#endif
      if (entry == -1)
      {
#ifdef DEBUG
         DcmAbort ("Read error in Dicom file");
#endif
         return (-1);
      }

      if (entry == -3)
      {
#ifdef DEBUG
         fprintf (stderr, 
            "** Requested field not found in Dicom file (%x), (%x)\n",
            sgroup, selem);
#endif
         return (-1);
      }

      if (entry == -4)
      {
#ifdef DEBUG
         DcmAbort ("Corrupted Dicom file");
#endif
         return (-1);
      }

      /*
       * If a data item is not found in the dictionary, let it go.
       */

      if (entry == -2)
      {
         FlushField (dcmfile, length);
         continue;
      }

      /*
       * Determine what to do next, depending on the field type.
       * SQ, call this routine recursively.
       * DL, this is the end of the sequence, there is no value, so return.
       */

        
      /*
       * Convert the type and make sure that its known.  The search
       * can continue.
       */

      vrtype = ConvertVRT (dicom_dd [entry] . dd_vrep);
      if (vrtype == XX)
      {
         FlushField (dcmfile, length);
         continue;
#ifdef DEBUG
         DcmAbort ("Unknown field type from Convert");
#endif
      }

      if (vrtype == DL)
      {
         /* Check to make sure that it is a sequence delimiter.  If an
          * item delimiter, something is wrong.  A sequence delimiter
          * ends the current sequence.  A data item delimiter ends
          * the current item, and shouldn't occur here.
          */

         if (element == 0xE0DD)  /* Sequence delimiter */
            return (0);

         if ((element == 0xE000) ||    /* Out of order delimiters */
             (element == 0xE00D))
            FlushField (dcmfile, length);
      }

      /*
       * Handle the special fields.
       *    Group Length
       *    Transfer syntax uid
       *    The OX fields
       *        Image pixel data
       *        Audio data
       *        Curve data
       *
       * The OX fields are OB or OW and may have special characteristics
       * depending on other item values in the file.  
       */

      if (element == 0x0000)
      {
         ProcessGroupLengthItem (dcmfile, group, vrtype, length);
         continue;
      }

      /* 
       * Transfer Syntax must be handled since it may affect
       * further file operations.
       */

      if ((group == 0x0002) && (element == 0x0010))
      {
         fread (value, (size_t) 1, (size_t) length, dcmfile);
         SetTransferSyntax (value);
         continue;
      }

      /*
       * If its an SQ, then this is a nested structure.  Such structures
       *  can have two formats, so call a function to sort this out and
       * it will again call this function. Send the length of the
       * current item, but the rest is insignificant.
       */

      if (vrtype == SQ)
         if (DcmHandleSQ (dcmfile, length) < 0)
            return (-1);
         else
            continue;

      /*
       * If it's not one of these, it is a normal field.  Ignore the
       * contents.  Strings and numeric values are handled differently.
       */

      switch (vrtype)
      {
         case OB:
         case OW:
         case OX:

            if ((group == 0x7FE0) && (element == 0x0010))
            {
               if ((sgroup == group) && (selem == element))
                  ExtractPixelData (dcmfile, length, dataval);
               else
                  FlushField (dcmfile, length);
               break;
            }

            if ((group == 0x5000) && (element == 0x200C))
            {
               if ((sgroup == group) && (selem == element))
                  ExtractAudioData (dcmfile, length, dataval);
               else
                  FlushField (dcmfile, length);
               break;
            }
   
            if ((group == 0x5000) && (element == 0x3000))
            {
               if ((sgroup == group) && (selem == element))
                  ExtractCurveData (dcmfile, length, dataval);
               else
                  FlushField (dcmfile, length);
               break;
            }

            ExtractByteString (dcmfile, vrtype, length, entry, dataval);
            break;
  
         default:
            ExtractValue (dcmfile, vrtype, length, entry, dataval);
      }   /* switch */

      if ((element == selem) && (group == sgroup))
         return (0);

   } while (1);
     

   /*******************************************/
   /* Commented out to avoid compiler warning.
    * This would never get executed
    */
   /*******************************************/
   /*return (-1);    /*Didn't find it  */
}
int DcmHandleSQ 
(  FILE             *dcmfile, 
   unsigned int     length
)
{
   unsigned short   group, element;
   unsigned long    ilength;
   dataval_T        dataval;
   unsigned long    bytecount;
   VRTYPE_T         vrtype;
   int              entry;

#ifdef DEBUG
         DcmAbort ("Handling SQ");
#endif

   /*
    * SQ types can either have an explicit length in bytes, or 
    * a delimiter at the end of the seqence of items.
    */

   if (length != 0xFFFFFFFF)   
   {
      bytecount = 0;
      while (bytecount < length)
      {
         entry = ReadItem (dcmfile, &group, &element, &ilength);
         if (entry < 0)
            return (entry);

         bytecount += 8;

         vrtype = ConvertVRT (dicom_dd [entry] . dd_vrep);
         if (vrtype == XX)
         {
            FlushField (dcmfile, length);
            continue;
         }

         ExtractValue (dcmfile, vrtype, ilength, entry, &dataval);
         free (dataval.dv_value);

         bytecount += ilength;
      }
   }
   else
   {
      do
      {
         entry = ReadItem (dcmfile, &group, &element, &ilength);
         if (entry < 0)
            return (entry);

         vrtype = ConvertVRT (dicom_dd [entry] . dd_vrep);
         if (vrtype == XX)
         {
            FlushField (dcmfile, length);
            continue;
         }

         ExtractValue (dcmfile, vrtype, ilength, entry, &dataval);
         if (dataval.dv_value != NULL)
            free (dataval.dv_value);

         if ((group == 0xFFFE) && (element == 0xE00D))
         {
#ifdef DEBUG
            DcmAbort ("Returning from SQ Handler");
#endif
            return (0);
         }

      } while (1);
   }
#ifdef DEBUG
   DcmAbort ("Returning from SQ Handler");
#endif
   return (0); 
}


void ExtractValue
(
   FILE            *dcmfile,
   VRTYPE_T        vrtype,
   unsigned long   length,
   int             entry,
   dataval_T       *dataval
)
{
   char             *cp, *ep, value [256];
   int              vm, ct, nitem;
   unsigned short   group, element;
   short            sv;
   long             *lp, lv;
   float            *fp, fv;
   double           dv;

   /*
    * Depending on the type, read the value and store.
    * The multiplicity of the item must be determined from the
    * size and length.
    */

   switch (vrtype)
   {
      case US:    /* two byte vals */
      case SS:
      case XS:
         dataval -> dv_type = INT_TYPE;
     
         vm = length / 2;
         dataval -> dv_num = vm;
         dataval -> dv_value = (void *) malloc (vm * sizeof (long));

         lp = dataval -> dv_value;
         for (ct = 0; ct < vm; ct++)
         {
            fread (&sv, (size_t) 2, (size_t) 1, dcmfile);
            ByteSwitch ((char *) &sv, 2);
            *lp++ = (long) sv;
         }
         break;

      case UL:    /* four byte ints */
      case SL:
         dataval -> dv_type = INT_TYPE;
     
         vm = length / 4;
         dataval -> dv_num = vm;
         dataval -> dv_value = (void *) malloc (vm * sizeof (long));

         lp = dataval -> dv_value;
         for (ct = 0; ct < vm; ct++)
         {
            fread (lp, (size_t) 4, (size_t) 1, dcmfile);
            ByteSwitch ((char*) lp, 4);
            lp ++;
         }
         break;

      case FL:
         dataval -> dv_type = FLOAT_TYPE;
     
         vm = length / 4;
         dataval -> dv_num = vm;
         dataval -> dv_value = (void *) malloc (vm * sizeof (float));

         fp = dataval -> dv_value;
         for (ct = 0; ct < vm; ct++)
         {
            fread (fp, (size_t) 4, (size_t) 1, dcmfile);
            ByteSwitch ((char*) fp, 4);
            fp ++;
         }
         break;

      case FD:            /* 8 byte val */
         dataval -> dv_type = FLOAT_TYPE;

         vm = length / 8;
         dataval -> dv_num = vm;
         dataval -> dv_value = (void *) malloc (vm * sizeof (double));

         fp = dataval -> dv_value;
         for (ct = 0; ct < vm; ct++)
         {
            fread (&dv, (size_t) 8, (size_t) 1, dcmfile);
            ByteSwitch ((char*) &dv, 8);
            *fp++ = (float) dv;
         }
         break;

      case DS:            /* decimal string - convert to float */
         /*
          * Multiplicity applies here as well, with the strings
          * separated by slashes ("\").  Ignored for now.
          * Note that a null character is added and included in the
          * length.
          */
         
         dataval -> dv_type = FLOAT_TYPE;
         fread (value, (size_t) 1, (size_t) length, dcmfile);
         value [length] = '\0';
        
         /* 
          * Count the number of separators.
          */

         cp = value;
         nitem = 0;
         do 
         {
            cp = strchr (cp, '\\');
            if (cp != NULL)
               while (*cp ++ == '\\');
            nitem ++; 
         } while (cp != NULL); 

         /*
          * Setup the structure for the data and parse it out.
          */

         dataval -> dv_num = nitem;
         dataval -> dv_value = (char *) malloc (nitem * sizeof (float));

         cp = value;
         ep = cp;
         fp = (float *) dataval -> dv_value;
         for (ct = 0; ct < nitem; ct++)  
         {
            while ((*ep != '\0') && (*ep != '\\'))
               ep++;
            *ep = '\0';
            *fp++ = (float) atof (cp);
 
            cp = ep + 1;
            while (*cp++ == '\\');
            ep = cp;
         } 

         break;

      default:            /* everything else is a string */
         /*
          * Multiplicity applies here as well, with the strings
          * separated by slashes ("\").  Ignored for now.
          * Note that a null character is added and included in the
          * length.
          */
         
         dataval -> dv_type = CHAR_TYPE;
         dataval -> dv_num = length + 1;
         dataval -> dv_value = (char *) malloc (length+1);

         cp = dataval -> dv_value;
         fread (cp, (size_t) 1, (size_t) length, 
            dcmfile);
         *(cp + length) = '\0';
         break;

   }   /* switch */


   /*
    * Store special data values that are needed for later decoding
    * such as pixel, audio and curve data.
    */


   if ((group == 0x0028) && (element == 0x0004))
      strcpy (pixel_data_g . px_photointerp, dataval -> dv_value);

   lp = (long *) dataval -> dv_value;
   group = dicom_dd [entry] . dd_group;
   element = dicom_dd [entry] . dd_element;
   if ((group == 0x0028) && (element == 0x0002))
      pixel_data_g . px_samples = (int) *lp;

   if ((group == 0x0018) && (element == 0x0086))
      pixel_data_g . px_necho = (int) atoi (dataval -> dv_value);

   if ((group == 0x0028) && (element == 0x0010))
      pixel_data_g . px_rows = (int) *lp;

   if ((group == 0x0028) && (element == 0x0011))
      pixel_data_g . px_cols = (int) *lp;

   if ((group == 0x0028) && (element == 0x0100))
      pixel_data_g . px_bits_alloc = (int) *lp;

   if ((group == 0x0028) && (element == 0x0101))
      pixel_data_g . px_bits_stored = (int) *lp;

   if ((group == 0x0028) && (element == 0x0102))
      pixel_data_g . px_high_bit = (int) *lp;

   if ((group == 0x0028) && (element == 0x0103))
      pixel_data_g . px_rep = (int) *lp;

   if ((group == 0x0028) && (element == 0x0106))
      pixel_data_g . px_smallest = (int) *lp;

   if ((group == 0x0028) && (element == 0x0107))
      pixel_data_g . px_largest = (int) *lp;

   return;
}

void ByteSwitch 
(
   char     *item, 
   int      len
)
{
   char      bytes [8], *bp, *ip;
   int       ct;

   /*
    * If not needed, don't switch
    */

   if ((file_info_g.machine_type == BIG_ENDIAN_TYPE) && 
       (file_info_g.transfer_syntax_type == EXP_BIG_ENDIAN))
      return;
   if ((file_info_g.machine_type == LITTLE_ENDIAN_TYPE) && 
       ((file_info_g.vr_type == IMP_LITTLE_ENDIAN) ||
        (file_info_g.vr_type == EXP_LITTLE_ENDIAN)))
      return;
   bp = bytes;
   ip = item;
   ip += len-1;
   for (ct = 0; ct < len; ct++)
      *bp ++ = *ip--;
   
   memcpy (item, bytes, len);
   return;
}

int GetDDEntry
(
   int    group, 
   int    element
)
{
   DD_ENTRY_T   entry;
   int          ct;

   /*
    * Search the data dictionary for the corresponding group and element
    * and return the index.  This should be done with a binary search, but
    * not yet.
    */

   ct = 0;
   do
   {
      if ((dicom_dd [ct] . dd_group == group) &&
          (dicom_dd [ct] . dd_element == element))
         return (ct);
      ct ++; 
   } while (dicom_dd [ct] . dd_vrep != NULL);
 
   return (-1);    /* Not found. */
}


VRTYPE_T ConvertVRT 
(
   char    *vstr
)
{
   /*
    * Do a lookup to convert from the string to the enum type.
    */

   switch (vstr[0])
   {
      case 'A':
         switch (vstr [1])
         {
            case 'E':
               return (AE);
            case 'S':
               return (AS);
            case 'T':
               return (AT);
         }
         break;

      case 'C':
         switch (vstr [1])
         {
            case 'S':
               return (CS);
         }
         break;

      case 'D':
         switch (vstr [1])
         {
            case 'A':
               return (DA);
            case 'L':
               return (DL);
            case 'S':
               return (DS);
            case 'T':
               return (DT);
         }

      case 'F':
         switch (vstr [1])
         {
            case 'L':
               return (FL);
            case 'D':
               return (FD);
         }
         break;

      case 'I':
         switch (vstr [1])
         {
            case 'S':
               return (IS);
         }
         break;

      case 'L':
         switch (vstr [1])
         {
            case 'O':
               return (LO);
            case 'T':
               return (LT);
         }
         break;

      case 'N':
         return (NA);

      case 'O':
         switch (vstr [1])
         {
            case 'B':
               return (OB);
            case 'W':
               return (OW);
            case 'X':
               return (OX);
         }
         break;

      case 'P':
         switch (vstr [1])
         {
            case 'N':
               return (PN);
         }
         break;

      case 'S':
         switch (vstr [1])
         {
            case 'H':
               return (SH);
            case 'L':
               return (SL);
            case 'Q':
               return (SQ);
            case 'S':
               return (SS);
            case 'T':
               return (ST);
         }

      case 'T':
         switch (vstr [1])
         {
            case 'M':
               return (TM);
         }
         break;

      case 'U':
         switch (vstr [1])
         {
            case 'I':
               return (UI);
            case 'L':
               return (UL);
            case 'S':
               return (US);
         }
         break;

      case 'X':
         switch (vstr [1])
         {
            case 'S':
               return (XS);
         }
         break;

   } /* switch */

   return (XX);
}

void FlushField 
(
   FILE            *dcmfile,
   unsigned long   inlen
)
{
   long       length;
   int        err;

   /*
    * If not passed in, read the length.
    */

   if (inlen == 0)
      return;

   if (inlen < 0)
   {
      fread (&length, (size_t) 4, (size_t) 1, dcmfile);
      ByteSwitch ((char*) &length, 4);
   }
   else
      length = inlen;

   
   err = fseek (dcmfile, length, SEEK_CUR);
#ifdef DEBUG
if (err > 0)
perror ("Error in fseek");
#endif

   return;
}


/* ============================================================> ReadItem
 *
 * Read a data item from the file and return then entry from the 
 * dictionary or
 *   -1 = read error
 *   -2 = illegal item type
 *   -3 = normal EOF
 *   -4 = out of place EOF
 *
 * ==========================================================================
 */

int ReadItem
(
   FILE            *dcmfile,
   short           *group,
   short           *element,
   unsigned long   *length
)
{
   char             type[3];
   short            tlen;
   int              entry, nbyte;
   VRTYPE_T         vrtype;

   if ((nbyte = fread (group, (size_t) 2, (size_t) 1, dcmfile)) < 0)
      return (-1);
   if (nbyte == 0)
      return (-3);
   ByteSwitch ((char*) group, 2);
   if ((nbyte = fread (element, (size_t) 2, (size_t) 1, dcmfile)) < 0)
      return (-1);
   if (nbyte == 0)
      return (-4);
   ByteSwitch ((char*) element, 2);

   if ((file_info_g.vr_type == IMPLICIT) && (*group != 0x0002))
   {
      /*
       * Implicit type.  Look it up in the dictionary to get the
       * type.  If not found, return -2.  Convert to vrtype and
       * read the length.  First, get the length.
       */
      
      if ((nbyte = fread (length, (size_t) 4, (size_t) 1, dcmfile)) < 0)
         return (-1);
      if (nbyte == 0)
         return (-4);
      ByteSwitch ((char*) length, 4);

      entry = GetDDEntry (*group, *element);
      if (entry < 0)
         return (-2);
      vrtype = ConvertVRT (dicom_dd [entry] . dd_vrep);
   }
   else
   {
      /*
       * Explicit type.  Two forms.  SQ, OW and OB have a two char
       * pad and then a long int length.  All others have a short
       * length field.
       */

      if (fread (type, (size_t) 1, (size_t) 2, dcmfile) <= 0)
         return (-1);
      if (nbyte == 0)
         return (-4);
      type [2] = '\0';
      vrtype = ConvertVRT (type);
      if ((vrtype == SQ) ||
          (vrtype == OW) ||
          (vrtype == OB))
      {
         /* 
          * Ignore two bytes and then get the length.
          */

         if (fread (type, (size_t) 1, (size_t) 2, dcmfile) <= 0)
            return (-1);
         if (nbyte == 0)
            return (-4);
         if (fread (length, (size_t) 4, (size_t) 1, dcmfile) <= 0)
            return (-1);
         if (nbyte == 0)
            return (-4);
         ByteSwitch ((char*) length, 2);
      }
      else
      {
         if (fread (&tlen, (size_t) 2, (size_t) 1, dcmfile) <= 0)
            return (-1);
         if (nbyte == 0)
            return (-4);
         ByteSwitch ((char*) &tlen, 2);
         *length = tlen;
      }
   }

   return (entry);
}

int GetTransferSyntax ()
{
   return (file_info_g.transfer_syntax_type);
}

void SetTransferSyntax 
(
    char         *value
)
{
   if (strcmp (value, DEFAULT_TRANSFER_UID) == 0)
   {
      file_info_g.transfer_syntax_type = IMP_LITTLE_ENDIAN;
      file_info_g.vr_type = IMPLICIT;
      file_info_g.jpeg_method = NOJPEG;
      return;
   }
   if (strcmp (value, EXPLICIT_LITTLE_ENDIAN_UID) == 0)
   {
      file_info_g.transfer_syntax_type = EXP_LITTLE_ENDIAN;
      file_info_g.vr_type = IMPLICIT;
      file_info_g.jpeg_method = NOJPEG;
      return;
   }
   if (strcmp (value, EXPLICIT_BIG_ENDIAN_UID) == 0)
   {
      file_info_g.transfer_syntax_type = EXP_BIG_ENDIAN;
      file_info_g.vr_type = EXPLICIT;
      file_info_g.jpeg_method = NOJPEG;
      return;
   }
   if (strcmp (value, LOSSLESS_JPEG_UID) == 0)
   {
      file_info_g.transfer_syntax_type = IMP_LITTLE_ENDIAN;
      file_info_g.vr_type = IMPLICIT;
      file_info_g.jpeg_method = LOSSLESS;
      return;
   }
   if (strcmp (value, LOSSY_JPEG_8BIT_UID) == 0)
   {
      file_info_g.transfer_syntax_type = IMP_LITTLE_ENDIAN;
      file_info_g.vr_type = IMPLICIT;
      file_info_g.jpeg_method = LOSSY_8BIT;
      return;
   }
   if (strcmp (value, LOSSY_JPEG_12BIT_UID) == 0)
   {
      file_info_g.transfer_syntax_type = IMP_LITTLE_ENDIAN;
      file_info_g.vr_type = IMPLICIT;
      file_info_g.jpeg_method = LOSSY12BIT;
      return;
   }

   return;
}

void ExtractByteString
(
   FILE             *dcmfile,
   VRTYPE_T         vrtype,
   unsigned long    length,
   int              entry,
   dataval_T        *dataval
)
{
   char        str [17];
   int         ct;

   /*
    * A string of type OX, OB or OW has been found.  Handle the ones desired
    * and ignore the rest.  OX is either OB or OW, depending on other
    * fields.  length gives the length in bytes.
    */

   switch (vrtype)
   {
      case OB:
 
      /*
       * If type OB, it is a simple byte string.  Read it and write the
       * first few characters.  If the length is unknown, then the string
       * is encoded as a series of fragments - each fragment starts with
       * an item delimiter that has the fragment length.
       */

      if (length < 17)
      {
         fread (str, (size_t) 1, (size_t) length, dcmfile);
      }
      else
      {
         for (ct = 0; ct < 17; ct ++)
         {
            fread (str, (size_t) 1, (size_t) 1, dcmfile);
         }
         for (ct = 17; ct < length; ct ++)
            fread (str, (size_t) 1, (size_t) 1, dcmfile);
      }
         break;

      case OW:     
         if ((length = 0xFFFFFFFF) || (length > 16))
         {
            FlushField (dcmfile, length);
         }
         else
         {
            fread (str, (size_t) 1, (size_t) length, dcmfile);
            str [length] = '\0';
         }
         break;

    
/*
      case OX:    
         FlushField (dcmfile, length);
         break;
*/
   }  /* switch */
   return;
}

int ExtractPixelData
(
   FILE             *dcmfile,
   unsigned long    length,
   dataval_T        *dataval
)
{
   char                byte;
   unsigned short      *sp, word;
   unsigned long       npixel;
   int                 ct, shift_const;

   /*
    * First, read the data.  Then figure out what form it is in.
    * This is basically a mess, but what can you do?
    */

   dataval -> dv_value = (void *) malloc (length);
   if (dataval -> dv_value == NULL)
   {
#ifdef DEBUG
fprintf (stderr, "Could not allocate memory for pixel data\n");
#endif
      return (-1);
   }
   fread (dataval -> dv_value, (size_t) 1, (size_t) length, dcmfile);


   /*
    * Pixel data can be sent in native or encapsulated form.  Native
    * form is OW, with specifics controlled by the fields in pixel_data_g.
    * Encapsulated is designated by one of the transfer syntax types
    * and may be fragmented or non-fragmented.  
    *
    * Currently, this is true only the native case is handled. 
    */

   sp = dataval -> dv_value;
   switch (file_info_g.transfer_syntax_type)
   {
      case 0:
      case 1:
      case 2:

      default:

         for (ct = 0; ct < length >> 1; ct++)
            ByteSwitch ((char *) sp++, 2);
         break;
            
   }

   /*
    * If the data is RGB as shown by the samples per pixel, it will
    * be handled differently.
    */

   if (pixel_data_g . px_samples > 4)
   {
#ifdef DEBUG
      fprintf (stderr, "Cannot handle 4 bytes per pixel\n");
#endif
      return (-1);
   }

   if (pixel_data_g . px_samples == 3)
   {
      return (ConvertRGBData (dataval, length));
   }
   else
      return (ConvertGrayData (dataval, length));
}

int ConvertGrayData 
(
   dataval_T      *dataval,
   unsigned long  length
)
{
   unsigned long       npixel;
   unsigned short      *sp;
   unsigned char       *cp;
   int                 shift_const, ct;

   /*
    * The data is stored in two byte quantities, but the bits_alloc,
    * bits_stored and high_bit fields determine the actual organization.
    * Find a conversion for the quantities and convert to simple
    * two byte per pixel values.
    */

   npixel = pixel_data_g . px_rows * pixel_data_g . px_cols;
 
   /*
    * Bits allocated could be 8 or 16.  16 bit words could contain
    * two pixels.
    */

   if (pixel_data_g . px_bits_alloc == 16)
      dataval -> dv_type = SHORT_TYPE;
   else
      dataval -> dv_type = CHAR_TYPE;
   shift_const = pixel_data_g . px_high_bit - 
         pixel_data_g . px_bits_stored + 1;
   if (shift_const == 0)
      return (0);

   /*
    * Convert the pixels as needed.
    */

   if (pixel_data_g . px_bits_alloc == 16)
   {
      sp = (unsigned short *) dataval -> dv_value;
      for (ct = 0; ct < npixel; ct ++)
         *sp++ >= shift_const;
   }
   else
   {
      cp = (unsigned char *) dataval -> dv_value;
      for (ct = 0; ct < npixel; ct ++)
         *cp++ >= shift_const;
   }
      
   return (0);
}

int ConvertRGBData 
(
   dataval_T      *dataval,
   unsigned long  length
)
{
   return (0);
}

int ExtractAudioData
(
   FILE             *dcmfile,
   unsigned long    length,
   dataval_T        *dataval
)
{
   /*
    * Maybe later.
    */

   return (0);
}

int ExtractCurveData
(
   FILE             *dcmfile,
   unsigned long    length,
   dataval_T        *dataval
)
{
   /*
    * Maybe later.
    */

   return (0);
}

int GetMachineType ()
{
   return (file_info_g.machine_type);
}

void SetMachineType ()
{

   short        word;
   char         *wp;

   /*
    * Write a word to memory and read it back.  Then set the machine
    * word organization as big or little endian.
    */

   word = 0x1234;
   wp = (char *) &word;

   if (*wp == 0x12)
      file_info_g.machine_type = BIG_ENDIAN_TYPE;   /* Big endian */
   else
      file_info_g.machine_type = LITTLE_ENDIAN_TYPE;   /* Little endian */
   
   return;
}


int DcmSearchPart10
(
   FILE             *dcmfile,
   unsigned short   sgroup,
   unsigned short   selem,
   dataval_T        *dataval
)
{
   char        str [256];

   /*
    * There is a 128 byte header, followed by a another "DICM"
    * key, and then, a normal DICOM 3.0 sequence of data items.
    * Read and ignore the next 124 bytes, then check for the 
    * key.  If everything is as predicted, process the file.
    */

   fread (str, (size_t) 1, (size_t) 124, dcmfile);

   fread (str, (size_t) 1, (size_t) 4, dcmfile);
   if (strncmp (str, "DICM", 4) != 0)
   {
#ifdef DEBUG
      DcmAbort ("ILLEGAL Part 10 FORMAT\n");
#endif
      return (-1);
   }

   /*
    * Encapsulation, at least initially, uses explicit little endian
    * protocol.
    */

   file_info_g.vr_type = EXPLICIT;
   file_info_g.transfer_syntax_type = EXP_LITTLE_ENDIAN;
   DcmSearchSequence (dcmfile, sgroup, selem, dataval, 0);

   return (0);
}


void ProcessGroupLengthItem 
(
   FILE             *dcmfile, 
   short            group, 
   VRTYPE_T         vrtype, 
   unsigned long    length
)
{
   unsigned short      stmp;
   unsigned long       ltmp;

   /*
    * Group lengths indicate the length of a group that can be
    * processed or ignored.  Read the length  and return. 
    */

   switch (vrtype)
   {
      case US:
         fread (&stmp, (size_t) 1, (size_t) 2, dcmfile);
         ByteSwitch ((char*) &stmp, 2);
         break;

      case UL:
         fread (&ltmp, (size_t) 1, (size_t) 4, dcmfile);
         ByteSwitch ((char*) &ltmp, 4);
         break;
   }

   return;
}

int DictionaryInit ()
{
   char        pathname [256], error [256];
   FILE        *dfile;

   strcpy (pathname, SERA_RESOURCES_PATH);
   strcat (pathname, "/"); 
   strcat (pathname, DICOM_DICTIONARY); 
 
   dfile = fopen (pathname, "r");
   if (dfile == NULL)
   {
      sscanf (error, "Opening dictionary file %s", pathname);
      perror (error); 
      exit (-1);
   }
   
/*
   dicom_dd = (DD_ENTRY_T *) malloc (sizeof (DD_ENTRY_T) * DICTIONARY_SIZE);
*/
   return (0);
}

FILE *DcmOpenInputFile (char *name)
{
   FILE    *dfile;
   char    str [256];

   dfile = fopen (name, "r");
   if (dfile == NULL)
      return (NULL);

   /*
    * Initialize the control variables.
    */

   SetMachineType ();

   /*
    * Check to see what type of file this is.  A standard NEMA format
    * or a DICOM 3.0 Part 10 format file.  Search for the
    * data item and set the file type.
    *
    * There are two formats - the old NEMA format which has no prefix and
    * no "DICM" key.  The new DICOM Part 10 format has a 128 byte
    * prefix and then the characters "DICM".
    */

   
   fread (str, (size_t) 1, (size_t) 128, dfile);
   fread (str, (size_t) 1, (size_t) 4, dfile);
   rewind (dfile);
   if (strncmp (str, "DICM", 4) == 0)
      file_info_g.file_type = DICOM_FILE;
   else
      file_info_g.file_type = NEMA_FILE;

   initialized_g = 1;    /* We're ready */

   file_info_g.vr_type = IMPLICIT;   /* Assume implicit encoding */
   file_info_g.transfer_syntax_type = IMP_LITTLE_ENDIAN;
   file_info_g.jpeg_method = NOJPEG; /* no jpeg */

   /*
    * Assumptions about the pixel data.
    */

   pixel_data_g . px_rows = 0;
   pixel_data_g . px_cols = 0;
   pixel_data_g . px_bits_alloc = 16;
   pixel_data_g . px_samples = 1;
   pixel_data_g . px_bits_stored = 16;
   pixel_data_g . px_high_bit = 15;
   pixel_data_g . px_rep = 0;
   pixel_data_g . px_photointerp [0] = '\0';
   pixel_data_g . px_smallest = 0;
   pixel_data_g . px_largest = 65535;

   return (dfile);
}

void DcmAbort
(
   char     *message
)
{
   fprintf (stderr, "** %s\n", message);
   return;
}

