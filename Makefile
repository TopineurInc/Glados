#
# EPITECH PROJECT, 2025
# G-FUN-500-LYN-5-1-glados-1
# File description:
# Makefile
#

EXEC = glados

BIN_PATH = $(shell stack path --local-install-root)

SRC = $(shell find src app -type f -name "*.hs")

all: $(EXEC)

$(EXEC): $(SRC)
	stack build
	echo $(BIN_PATH)
	mv $(BIN_PATH)/bin/$(EXEC)-exe $(EXEC)

clean:
	stack clean
	rm -f $(EXEC)

fclean: clean
	rm -rf $(EXEC) .stack-work/ dist/ dist-newstyle/

re: fclean all

tests_run:
	stack test
