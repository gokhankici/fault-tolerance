TO_REMOVE= *.beam *.pdf *.tex *.html

.PHONY: clean test

all: test

test:
	dialyzer -Wrace_conditions -Werror_handling -Wunknown --src -r .

clean:
	rm -f $(TO_REMOVE)

