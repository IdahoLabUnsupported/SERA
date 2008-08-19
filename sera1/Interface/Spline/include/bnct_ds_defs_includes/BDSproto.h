/* prototypes for BDS */

/* SLICE **************/

SLICE_LINK
BDSNewSlice();

void
BDSInitSlice(SLICE_LINK);

void
BDSFreeSlice(SLICE_LINK);

void
BDSFreeSliceNodes(SLICE_LINK);

void
BDSPrintSlice(SLICE_LINK, int);

void
BDSAddSlice(SLICE_LINK, ENV_LINK);

void
BDSDumpSlice(SLICE_LINK, FILE *, BDS_BOOL);

/* BODY ***************/

BODY_LINK
BDSNewBody();

void
BDSInitBodyList(BODY_LIST_LINK);

void
BDSFreeBody(BODY_LINK);

void
BDSFreeBodyNodes(BODY_LINK);

void
BDSPrintBody(BODY_LINK, int);

/* SLICE_LIST *********/

SLICE_LIST_LINK
BDSNewSliceList();

void
BDSInitSliceList(SLICE_LIST_LINK);

void
BDSFreeSliceList(SLICE_LIST_LINK);

void
BDSFreeSliceListNodes(SLICE_LIST_LINK);

void
BDSPrintSliceList(SLICE_LIST_LINK, int);


/* BODY_LIST **********/

BODY_LIST_LINK
BDSNewBodyList();

void
BDSInitBodyList(BODY_LIST_LINK);

void
BDSFreeBodyList(BODY_LIST_LINK);

void
BDSFreeBodyListNodes(BODY_LIST_LINK);

void
BDSPrintBodyList(BODY_LIST_LINK, int);

/* POINT***************/

POINT_LINK
BDSNewPoint();

void
BDSInitPoint(POINT_LINK);

void
BDSFreePoint(POINT_LINK);

void
BDSFreePointNodes(POINT_LINK);

void
BDSPrintPoint(POINT_LINK, int);

/* POLYLINE ***********/

POLYLINE_LINK
BDSNewPolyline();

void
BDSInitPolyline(POLYLINE_LINK);

void
BDSFreePolyline(POLYLINE_LINK);

void
BDSFreePolylineNodes(POLYLINE_LINK);

void
BDSPrintPolyline(POLYLINE_LINK, int);

void
BDSPrintPolylineNodes(START_END_LINK, int);

/* CURVE **************/

CURVE_LINK
BDSNewCurve();

void
BDSInitCurve(CURVE_LINK);

void
BDSFreeCurve(CURVE_LINK);

void
BDSFreeCurveNodes(CURVE_LINK);

void
BDSPrintCurve(CURVE_LINK, int);

/* CNURB **************/

CNURB_LINK
BDSNewCnurb();

void
BDSInitCnurb(CNURB_LINK);

void
BDSFreeCnurb(CNURB_LINK);

void
BDSFreeCnurbNodes(CNURB_LINK);

void
BDSPrintCnurb(CNURB_LINK, int);

/* SNURB **************/

void BDSDrawSnurb (Display *, Drawable, GC, BDS_MATRIX *, RECON_BODY_LINK);

void transform_pts (BDS_REAL [][4], BDS_REAL *, BDS_REAL *, int, int);
void TransformPoint (BDS_REAL [][4], BDS_REAL [], BDS_REAL []);
void mat_vect_mult (BDS_REAL [][4], BDS_REAL *, BDS_REAL *);
 
/*
SNURB_LINK
BDSNewSnurb();

void
BDSInitSnurb(SNURB_LINK);

void
BDSFreeSnurb(SNURB_LINK);

void
BDSFreeSnurbNodes(SNURB_LINK);

void
BDSPrintSnurb(SNURB_LINK, int);
*/

/* RECON **************/

RECON_LINK
BDSNewRecon();

void
BDSInitRecon(RECON_LINK);

void
BDSFreeRecon(RECON_LINK);

void
BDSFreeReconNodes(RECON_LINK);

void
BDSPrintRecon(RECON_LINK, int);

/* RECON_BODY *********/

RECON_BODY_LINK
BDSNewReconBody();

void
BDSInitReconBody(RECON_BODY_LINK);

void
BDSFreeReconBody(RECON_BODY_LINK);

void
BDSFreeReconBodyNodes(RECON_BODY_LINK);

void
BDSPrintReconBody(RECON_BODY_LINK, int);

/* SHOT_RAY ***********/

SHOT_RAY_LINK
BDSNewShotRay();

void
BDSInitShotRay(SHOT_RAY_LINK);

void
BDSFreeShotRay(SHOT_RAY_LINK);

void
BDSFreeShotRayNodes(SHOT_RAY_LINK);

void
BDSPrintShotRay(SHOT_RAY_LINK, int);

/* ATTRIBUTE  *********/

ATTRIBUTE_LINK
BDSNewAttribute();

void
BDSInitAttribute(ATTRIBUTE_LINK);

void
BDSFreeAttribute(ATTRIBUTE_LINK);

void
BDSFreeAttributeNodes(ATTRIBUTE_LINK);

void
BDSPrintAttribute(ATTRIBUTE_LINK, int);

/* IP_HISTORY *********/

IP_HISTORY_LINK
BDSNewIpHistory();

void
BDSInitIpHistory(IP_HISTORY_LINK);

void
BDSFreeIpHistory(IP_HISTORY_LINK);

void
BDSFreeIpHistoryNodes(IP_HISTORY_LINK);

void
BDSPrintIpHistory(IP_HISTORY_LINK, int);

/* utilities **********/



BDS_VOID_PTR
BDSGetMemory(int, char *);

BDS_COORD
BDSAssignCoord(BDS_REAL, BDS_REAL, BDS_REAL);

BDS_COORD_4D
BDSAssignCoord4D(BDS_REAL, BDS_REAL, BDS_REAL, BDS_REAL);


void
BDSFreeList(BDS_VOID_PTR);

void
BDSFreeEnv(ENV_LINK);

void
BDSFreeXenv(X_ENV_LINK);

void
BDSFreeLenv(LIST_ENV_LINK);

void
BDSFreeCenv(CURRENT_ENV_LINK);

void
BDSFreeSubdiv(BDS_SURFACE_LINK);

void
BDSFreeTriangle(BDS_TRIANGLE_LINK);

void
BDSFreeBox(BDS_BOUNDING_BOX);

void
BDSTab(int);

void
BDSPrintTool(BDS_IPTOOL);

BDS_MATRIX *
BDSAssignMatrix(BDS_REAL, BDS_REAL, BDS_REAL, BDS_REAL,
		BDS_REAL, BDS_REAL, BDS_REAL, BDS_REAL,
		BDS_REAL, BDS_REAL, BDS_REAL, BDS_REAL,
		BDS_REAL, BDS_REAL, BDS_REAL, BDS_REAL);

void
BDSInitMatrix(BDS_MATRIX *);

BDS_MATRIX *
BDSNewMatrix();

void
BDSPrintMatrix(BDS_MATRIX *, int);

void
BDSCopyMatrix(BDS_MATRIX *, BDS_MATRIX *);

BDS_COORD_4D
BDSTransform(BDS_COORD_4D, BDS_MATRIX *);

void
BDSRotateX(BDS_MATRIX *, BDS_REAL);

void
BDSScale(BDS_MATRIX *, BDS_REAL, BDS_REAL, BDS_REAL);

void
BDSTranslate(BDS_MATRIX *, BDS_REAL, BDS_REAL, BDS_REAL);


void
BDSPrintNodeType(BDS_NODE_TYPE);

int
BDSListAdd(BDS_VOID_PTR, BDS_VOID_PTR, START_END_LINK);

START_END_LINK
BDSInitStartEnd();

void
BDSCrossLink(SLICE_LINK, BODY_LINK);

BDS_STRING
BDSString(BDS_STRING);

POINT_LINK 
BDSPointEnd(SLICE_LINK);

START_END_LINK 
BDSPointList(SLICE_LINK, BODY_LINK);

BDS_INT
BDSCountPoints(START_END_LINK);

void
BDSFree(BDS_VOID_PTR);

POINT_LINK
BDSCopyCross(SLICE_LINK, BODY_LINK, CURVE_LINK);

void
BDSRotateX(BDS_MATRIX *, BDS_REAL);

void
BDSRotateY(BDS_MATRIX *, BDS_REAL);

void
BDSRotateZ(BDS_MATRIX *, BDS_REAL);

void
BDSTranslate(BDS_MATRIX *, BDS_REAL, BDS_REAL, BDS_REAL);

void
BDSScale(BDS_MATRIX *, BDS_REAL, BDS_REAL, BDS_REAL);

struct snurb *
BDSAddSnurb(RECON_BODY_LINK);

struct surface_type *
BDSAddSurface(RECON_BODY_LINK);

BDS_INT
BDSCount(START_END_LINK);

BDS_INT
BDSFlatCap(RECON_BODY_LINK);

BDS_INT
BDSRoundCap(RECON_BODY_LINK);

SLICE_LINK 
BDSFindMinZ(RECON_BODY_LINK);

SLICE_LINK 
BDSFindMaxZ(RECON_BODY_LINK);

BDS_COORD
BDSCentroid(BODY_LINK, SLICE_LINK);

void
BDSAddCapCurve(BODY_LINK, RECON_BODY_LINK, SLICE_LINK, BDS_BOOL, BDS_COORD);

int
BDSListAddFront(BDS_VOID_PTR, START_END_LINK);

BDS_BOOL
BDSEnvEmpty(ENV_LINK);

void
BDSBuildPolylines(RECON_LINK, RECON_BODY_LINK);

void
BDSBuildPolyline(BDS_INT, SLICE_LIST_LINK, START_END_LINK, 
                 RECON_LINK, RECON_BODY_LINK);

void
BDSExit(char *);
