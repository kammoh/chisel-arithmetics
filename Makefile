
PHONY: bloop, idea, bsp, clean


MILL ?= ./mill
BLOOP ?= bloop

bloop:
	$(MILL) --import ivy:com.lihaoyi::mill-contrib-bloop:  mill.contrib.bloop.Bloop/install

idea:
	$(MILL) -j 0 mill.idea.GenIdea/idea

bsp:
	$(MILL) -j 0 mill.bsp.BSP/install

build:
	$(MILL) _.compile

clean:
	# $(BLOOP) clean ntt --propagate
	$(MILL) __.clean
	$(MILL) __.shutdown
	$(RM) -r out/ .bloop/ .idea/ .metals/
	# jps -l |grep bloop.Server | awk '{print $1}' | xargs kill -TERM
	# jps -l |grep mill.runner.MillServerMain | awk '{print $1}' | xargs kill -TERM
	$(RM) -r out/ .bloop/ .idea/ .metals/