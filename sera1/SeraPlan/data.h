/*
 *  This structure holds the information needed for the
 *  multifield combination process.
 */

#define  MAX_FRACTIONS   4
#define  MAX_FIELDS      6
#define  MAX_NUM        20
#define  MAX_ID         50
#define  MAX_FILE      256

typedef struct {

   int FRACTIONS;
   int FIELDS;
   int field_ACTIVE[MAX_FRACTIONS][MAX_FIELDS];
   int fraction_ACTIVE[MAX_FRACTIONS];
   int fraction_same[MAX_FRACTIONS];

   char patient_ID[MAX_ID];
   char treat_date[MAX_ID];
   char save_dir[MAX_FILE];
   char field_file[MAX_FRACTIONS][MAX_FIELDS][MAX_FILE];
   char base_field_file[MAX_FIELDS][MAX_FILE];

   float field_B10[MAX_FRACTIONS][MAX_FIELDS];
   float field_EXPOSURE[MAX_FRACTIONS][MAX_FIELDS];
   float field_GAMMA[MAX_FRACTIONS][MAX_FIELDS];

   float fraction_BAVE[MAX_FRACTIONS];
   float fraction_BEFF[MAX_FRACTIONS];
   float fraction_EXPOSURE[MAX_FRACTIONS];
   float fraction_WEIGHT[MAX_FRACTIONS];

   float total_BAVE;
   float total_BEFF;
   float total_EXPOSURE;

} plan_data;
