PROJECT = erl_to_png
PROJECT_DESCRIPTION = Render Erlang source to a PNG file
PROJECT_VERSION = 0.0.1
DEPS = erl_png erl_freetype

dep_erl_png = git https://github.com/cwmaguire/erl_png master
dep_erl_freetype = git https://github.com/cwmaguire/erl_freetype master

ERLC_OPTS = -I deps/erl_png/include/

include erlang.mk
