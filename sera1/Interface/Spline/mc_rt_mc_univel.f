       program interface
       integer*4 dt
       real t(2), tim
       real rdir(3)
       double precision dist
       integer*4 this, next
 
c---      initialize rt variables 
          call bnctin
c---      stop
          x = 0.0
          y = 0.0
          z = 0.5
          rdir(1) = 1.0
          rdir(2) = 0.0
          rdir(3) = 0.0   
          print *,"Enter number of starting points"
          read *, npnts
          dt = 0
          do i = 1, npnts
            print *,"Enter x y z cos(alpha) cos(beta) cos(gamma)"
            read *, x, y, z, rdir(1), rdir(2), rdir(3)
            call bnctray(this, next, dist, x, y, z, rdir,t_eps)
          end do
          dt = 0
          tim = FLOAT(dt)
          print *, "Elapsed time = ", tim, " for ",npnts," tracks"
       stop
       end
c------------------------------------------------------------------------
c
c  mc_ray_mc.f:	      - FORTRAN wrapper between the RAFFLE Monte Carlo
c			code and the alpha-1 ray-tracing environment.
c  Calling sequence:
c			Calls init_ray_environ from the ray_trace.c file
c			to set up ray tracing environment which in turn
c			will communicate with the actual ray tracing functions.
c			The file name from bnct_edit's surface object dump
c			is passed. Nothing is returned directly to this wrapper
c 			by init_ray_environ . Called once for each problem.
c
c			Passes the particle coordinates and direction from
c			subroutine DTB to interrogate_geometry of file
c			ray_trace.c which returns intersection coordinates
c			and material information. Called many times.
c
c IMPORTANT:		It is important, at least for now, that the functions
c			init_ray_environ and interrogate_geometry be in the same
c			file because of variables env, root_world, near_bounds, 
c 			and far_bounds have file scope. This should be
c			changed !
c
c Author:		Dan Wessol
c			Idaho National Engineering Lab
c			BNCT Program
c			(208) 526-9046
c			dew@inel.gov
c
c Date:			May 7, 1990
c
c
c------------------------------------------------------------------------

c------------------------------------------------------------------------
c
c			B N C T I N
c
c	Calls init_ray_environ with the file name of the bnct_edit-produced
c 	surface object dump file. Nothing returned back but the ray tracing
c	file scope variables are set up.
c 	Reads one card which has the surface dump file name then calls
c       init_ray_environ. Called just one time for each problem.
c
c
c------------------------------------------------------------------------
      subroutine bnctin
      character*72 file
      character*20 exclude
      integer*4 openstat
      integer*4 nest
      double precision t_eps
C
C-----read in the name of the surface object dump file
C            
      open (3,file='surface_path',status='old',err=201)
      read( 3, 101 ) file
      close(3)
  101 format(a72)

C
C---- initializes variables env, root_world, near_bounds, and far_bounds
C---- for subsequent calls to the ray tracer (see code in ray_trace.c)
C
c      print *,"How deep of a subdivision tree?  Recommend < 16."
c      read *, nest
c      print *,"Enter t-eps"
c      read *, t_eps
       exclude = ''
c      print *, "String = ", exclude

      nest = 0
      t_eps = 0.0

      call init_ray_environ( nest, t_eps, file, exclude)

      return

  201  print *, "Could not open surface_path"
       stop

      end
c------------------------------------------------------------------------
c
c			B N C T R A Y
c
c	Called from dtb2.f
c
c   Inputs -
c	x,y,z	Starting point
c	rdir(3)	Direction vector, with unit length
c
c   Outputs -
c	this	Material ID of the region containing the start point.
c	next	Material ID of the next region along the ray.
c	dist	Distance to the boundary with the next region.
c
c------------------------------------------------------------------------
      subroutine bnctray(this, next, dist, x,y,z,rdir)

c
c-- cross-language communication with interrogate_geometry. Check to see if
c-- the point_type definition is double or single ! It's double.
c
      real rdir(3)
      double precision dpt(3), ddir(3), endpt(3)
      double precision dist, matno
      integer*4 this, next, miss
      integer hit_flag

      dpt(1)=x
      dpt(2)=y
      dpt(3)=z
      ddir(1)=rdir(1)
      ddir(2)=rdir(2)
      ddir(3)=rdir(3) 
      miss = 0
      do while(miss .eq. 0)
        call interrogate_geometry( dpt, ddir, dist, this, next, miss)
c        if (dist.eq.0.0) miss = 1
	if(miss.eq.0) then
           print *, "distance to boundary = ", dist
           print *, "this material number = ", this
           print *, "next material number = ", next
           print *, "miss flag = ", miss
	   dpt(1) = dpt(1) + dist * ddir(1)
	   dpt(2) = dpt(2) + dist * ddir(2)
	   dpt(3) = dpt(3) + dist * ddir(3)
        else
	   print *, "particle escaped system on next track"
        end if
      end do
      return
      end



