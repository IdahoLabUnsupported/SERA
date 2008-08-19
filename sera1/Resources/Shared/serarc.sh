# sets up the enviroment varaiables for SERA_Rtpe. There are
# several ways to make this file take effect. See your system
# administrator for the best way.
export SERA_HOME=$HOME/sera1

# SERA_PATH sets the top level directory for the SERA_Rtpe source code.
# Only required by developers.
export SERA_PATH=$SERA_HOME

# SERA_RESOURCES locates the X11 resource files required by SERA_Rtpe and
# its ancillary functions like xcontours. Also contains materials and
# color files for SERA_Rtpe.
export SERA_RESOURCES=$SERA_HOME/Resources
#setenv SERA_LIB $SERA_HOME/linux/lib

# SERAPATH locates the top level directory for rtt_MC.
export SERAPATH=$SERA_HOME

#Floyd
export SERAMC=SeraMC
export SERAMC_PATH=$SERA_HOME/$SERAMC

# SERA_IMAGES locates the image files.
export SERA_IMAGES=$HOME/images

# SERA_MASKS locates the image mask files.
export SERA_MASKS=.

# Environment variables required for context sensitive help.
export SERA_HELP=$SERA_HOME/Docs/dialog_help
export FILES_MANUAL_PATH=$SERA_HOME/Docs/Manuals/sera1/ 
export NET_MANUAL_PATH=http://www.cs.montana.edu/~bnct/manual/

if test $DISPLAY;
then
    xrdb -merge $SERA_RESOURCES/app-defaults/SeraCalc;
    xrdb -merge $SERA_RESOURCES/app-defaults/SeraDose;
    xrdb -merge $SERA_RESOURCES/app-defaults/SeraImage;
    xrdb -merge $SERA_RESOURCES/app-defaults/SeraMenu;
    xrdb -merge $SERA_RESOURCES/app-defaults/SeraPlan;
fi
