docs/index.html: Main.elm
	elm make --optimize --output=$@ $<
