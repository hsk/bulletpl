all:
	swipl load.pl
test:
	swipl syntax_test.pl
diff:
	meld examples bulletpl
diff2:
	code --diff examples bulletpl
