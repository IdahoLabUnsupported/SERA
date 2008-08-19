GC              gc;

XtAppContext    context;

Widget          toplevel,                   /* toplevel shell               */
                rowcol,                     /* row of button widgets        */
                form,                       /* form widget                  */
                clear_button,               /* push to restart              */
                brl_draw_button,            /* push to brl_draw bodies      */
                eval_draw_button,            /* push to brl_draw bodies      */
                selective_draw_button,      /* push to bring up choices     */
                brl_oslo_button,            /* push to brl-subdivide curve  */
                fire_phaser_button,         /* push to fire a ray           */
                peterson_ray_button,        /* push to fire a ray, 
					       peterson-style               */
                print_nurb_button,          /* push to print nurb info      */
                subdivide_button,           /* push to subdivide surface    */
                flat_cap_button,            /* push to put flat caps on */
					    /* surfaces */
                point_cap_button,           /* push to put pointed caps */
					    /* on surfaces */
                split_cap_button,           /* push to subdivide surface    */
                test_button,                /* push to do some test         */
                point_eval_button,          /* P(u,v) = ?                   */ 
                fire_uv_button,             /* fire a ray at point (u,v) */
					    /*    */
                free_subsrfs_button,        /* Frees all subsurfaces */
                exit_button,                /* push to exit                 */
                selective_drawlist_dialog,  /* push to draw some bodies     */
                sep1,                       /* seps buttons from sliders    */
                sep2,                       /* separates sliders from ??    */
                label,                      /* label for PRP sliders        */
                vrp_x_slider,
                vrp_y_slider,
                vrp_z_slider,
                drawing_area,               /* where spline will be drawn.  */
                menu_bar,                   /* what else, the menu bar      */
                file_selector,              /* dialog box to get file name  */
                open_file_option,           /* Open a file.                 */
                close_file_option,          /* Close a file.                */
                quit_file_option,           /* Quits program.               */
                open_nurb_option,           /* Spline will be open          */
                floating_nurb_option,       /* Spline will by regular.      */
                periodic_nurb_option,       /* Spline will be closed curve. */
                first_order_option,         /* Basis functions will be 
					       constant.                    */
                second_order_option,        /* Basis functions will be 
					       linear.                      */
                third_order_option,         /* Basis functions will be 
					       quadratic.                   */
                fourth_order_option,        /* Basis functions will be 
					       cubic.                       */
                w;                          /* Generic widget function.     */


Pixmap          pixmap;               /* Used to handle expose events.      */

Display        *p_disp;               /* Identifies the X display.          */

XmStringCharSet char_set = XmSTRING_DEFAULT_CHARSET;

