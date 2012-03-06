# $Id: Makefile,v 1.1 2011/06/28 21:04:23 neswold Exp $

override APPLICATION := states_dev
override DESCRIPTION := STATE front-end driver
override VSN := 1.0
override MODULES := states_dev
override APPLICATIONS := kernel stdlib acnet

include ${ERL_LIBS}/erlang.mk
