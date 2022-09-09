override APPLICATION := states_dev
override DESCRIPTION := STATE front-end driver
override VSN := 2.1
override MODULES := states_dev
override APPLICATIONS := acnet
override REGISTERED := fsmset state

include ${ERL_LIBS}/erlang.mk

