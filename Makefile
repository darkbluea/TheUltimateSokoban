BINARY_PATH		:=	$(shell stack path --local-install-root)
NAME			=	the-ultimate-sokoban

all:
	stack build
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

# stack build --pedantic

clean:
	stack clean

fclean: clean
	stack purge
	rm -f $(NAME)

re: fclean all

tests_run:
	stack test

.PHONY: all clean fclean re tests_run
