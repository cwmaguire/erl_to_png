PROJECT = erl_to_png
PROJECT_DESCRIPTION = Render Erlang source to a PNG file
PROJECT_VERSION = 0.0.1
DEPS = erl_to_html erl_freetype erl_png

dep_erl_to_html = git https://github.com/cwmaguire/erl_to_html master
dep_erl_freetype = git https://github.com/cwmaguire/erl_freetype master
dep_erl_png = git https://github.com/cwmaguire/erl_png master

ERLC_OPTS = -I deps/erl_png/include/
TEST_ERLC_OPTS = -I deps/erl_png/include/
CT_OPTS = -include /Users/cmaguire/dev/erl_png/include

include erlang.mk
