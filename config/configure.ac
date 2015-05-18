#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.60])
AC_INIT(grew, 1.0.0, bruno.guillaume@inria.fr)

AC_ARG_ENABLE(
	[config],
 	[AS_HELP_STRING([--enable-config],[Enable configuration check (default is yes)])],
	[config=$enableval],
	[config=yes])
	
AC_ARG_ENABLE(
	[dev],
 	[AS_HELP_STRING([--enable-dev],[Enable grew dev version (default is no)])],
	[dev=$enableval],
	[dev=no])

AC_ARG_ENABLE(
	[cluster],
 	[AS_HELP_STRING([--enable-cluster],[Enable cluster (default is no)])],
	[cluster=$enableval],
	[cluster=no])

AC_ARG_ENABLE(
        [gui],
	[AS_HELP_STRING([--enable-gui],[build GUI extension (default is yes)])],
	[with_gui=$enableval],
	[with_gui=yes])

AC_ARG_ENABLE(
	[dep2pict],
 	[AS_HELP_STRING([--enable-dep2pict],[build with dep2pict even if gui is disabled (default is yes)])],
	[dep2pict=$enableval],
	[dep2pict=yes])
	
AC_ARG_ENABLE(
	[debug],
 	[AS_HELP_STRING([--enable-debug], [Display debug trace])],
	[with_debug=$enableval],
	[with_debug=no])

AC_ARG_ENABLE(
	[warning],
 	[AS_HELP_STRING([--enable-warning], [Display warning trace])],
	[with_warning=$enableval],
	[with_warning=no])

AC_ARG_ENABLE(
	[message],
 	[AS_HELP_STRING([--enable-message], [Display message trace])],
	[with_message=$enableval],
	[with_message=yes])

AC_ARG_ENABLE(
	[info],
 	[AS_HELP_STRING([--enable-info], [Display info trace])],
	[with_info=$enableval],
	[with_info=yes])

if test "$config" = yes ; then

   	# Checks for programs.
	AC_CHECK_PROGS(OCAMLC,ocamlc.opt ocamlc,no)
	if test "$OCAMLC" = no ; then
		AC_MSG_ERROR(Cannot find ocamlc)
	fi
        AC_SUBST(OCAMLC)

        AC_CHECK_PROGS(OCAMLOPT,ocamlopt.opt ocamlopt,no)
        if test "$OCAMLOPT" = no ; then
                AC_MSG_ERROR(Cannot find ocamlopt)
        fi
        AC_SUBST(OCAMLOPT)

	# Checks for libraries.
	AC_MSG_CHECKING(for ocaml library path)
	OCAMLLIB=$($OCAMLC -where)
	AC_MSG_RESULT($OCAMLLIB)
	if test "$OCAMLLIB" = ""; then
		AC_MSG_ERROR(Cannot find ocaml library path)
	fi
	AC_SUBST(OCAMLLIB)

	# Look for ocamlfind
	AC_PATH_PROG(OCAMLFIND,ocamlfind,no)
	if test "$OCAMLFIND" = no ; then
		AC_MSG_ERROR(Cannot find ocamlfind)
	fi
        AC_SUBST(OCAMLFIND)

	m4_include(ac_lib_checking.m4)

	AC_LIB_CHECKING(Xml-light,xml-light,xml-light,xml-light,XmlParser,"",true)
	AC_SUBST(XMLLIGHT_INCLUDE,$LIB_INCLUDE)
	
	AC_LIB_CHECKING(ANSITerminal,ANSITerminal,ANSITerminal,ANSITerminal,ANSITerminal,"",true,true)
	AC_SUBST(ANSITERMINAL_INCLUDE,$LIB_INCLUDE)
	
	AC_LIB_CHECKING(Log,log,log,log,Log,"",true,true)
	AC_SUBST(LOG_INCLUDE,$LIB_INCLUDE)
		
	AC_LIB_CHECKING(Itext,itext,itext,itext,Itext,"",true,true)
	AC_SUBST(ITEXT_INCLUDE,$LIB_INCLUDE)

# DEP2PICT is available only if CAIRO and Dep2pict are found *)
        if test "x$dep2pict" = xno ; then
        	AC_SUBST(USE_DEP2PICT,no)
        else

        	AC_LIB_CHECKING(Cairo,cairo2,cairo2,cairo2,Cairo,"",false,true)
            AC_SUBST(CAIRO_INCLUDE,$LIB_INCLUDE)
	
            AC_LIB_CHECKING(Dep2pict,dep2pict,dep2pict,dep2pict,Dep2pict,"",false,true)
	        AC_SUBST(DEP2PICT_INCLUDE,$LIB_INCLUDE)

		    AC_LIB_CHECKING(Camomile,camomile,camomile,camomile,"CamomileLibraryDefault","bigarray.cma",true)
		    AC_SUBST(CAMOMILE_INCLUDE,$LIB_INCLUDE)

            if test "$CAIRO_INCLUDE" = "no" ;
            then AC_SUBST(USE_DEP2PICT,no)
            else
                if test "$DEP2PICT_INCLUDE" = "no";
                then AC_SUBST(USE_DEP2PICT,no)
                else AC_SUBST(USE_DEP2PICT,yes)
                fi
	        fi
        fi
	AC_SUBST(USE_DEP2PICT)
	
# GUI is available only if LALGTK and LABLWEBKIT are found *)
        if test "x$with_gui" = xno ; then
        	AC_SUBST(BUILD_GUI,no)
        else
                AC_LIB_CHECKING(Lablgtk2,lablgtk2,lablgtk2,lablgtk,GMain,"",false,true)
	        AC_SUBST(LABLGTK_INCLUDE,$LIB_INCLUDE)		

	        AC_LIB_CHECKING(Lablwebkit,lablwebkit,lablwebkit,lablwebkit,GWebView,"$LABLGTK_INCLUDE lablgtk.cma",false,true)
	        AC_SUBST(LABLWEBKIT_INCLUDE,$LIB_INCLUDE)

                if test "$LABLGTK_INCLUDE" = "no" ;
                then AC_SUBST(BUILD_GUI,no)
                else
                        if test "$LABLWEBKIT_INCLUDE" = "no";
                        then AC_SUBST(BUILD_GUI,no)
                        else AC_SUBST(BUILD_GUI,yes)
                        fi
	        fi
        fi
	AC_SUBST(BUILD_GUI)

	
	


	
  BUILD_CLUSTER="no"
	if test "$cluster" = "yes"; then
  	AC_LIB_CHECKING(Cluster,cluster,cluster,cluster,Cluster,"",true,true)
	  AC_SUBST(CLUSTER_INCLUDE,$LIB_INCLUDE)
	  BUILD_CLUSTER="yes"
	  if test "$CLUSTER_INCLUDE" = "no"; then
	    BUILD_CLUSTER="no"
	  fi
	fi
  AC_SUBST(BUILD_CLUSTER)



	if test "x$dev" = xno ; then
		AC_LIB_CHECKING(Libgrew,libgrew,libgrew,libgrew,Libgrew,"",true)
		AC_SUBST(LIBGREW_INCLUDE,$LIB_INCLUDE)
	else
		AC_SUBST(DEV,yes)
		AC_LIB_CHECKING(Libgrew,libgrew,libgrew_dev,libgrew,Libgrew,"",true)
		AC_SUBST(LIBGREW_INCLUDE,$LIB_INCLUDE)
	fi
	
	if test "x$with_debug" != xno ; then
		AC_SUBST(DEBUG,yes)
	else
		AC_SUBST(DEBUG,no)
	fi
	if test "x$with_warning" != xno ; then
		AC_SUBST(WARNING,yes)
	else
		AC_SUBST(WARNING,no)
	fi
	if test "x$with_message" != xno ; then
		AC_SUBST(MESSAGE,yes)
	else
		AC_SUBST(MESSAGE,no)
	fi
	if test "x$with_info" != xno ; then
		AC_SUBST(INFO,yes)
	else
		AC_SUBST(INFO,no)
	fi

	AC_SUBST(LIB)

fi

AC_SUBST(SET_MAKE)
AC_CONFIG_FILES(config/Makefile Makefile src/Makefile)
AC_PROG_MAKE_SET
AC_OUTPUT

cd config && make infos
echo ""

