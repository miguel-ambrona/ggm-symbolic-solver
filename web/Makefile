all: get-bootstrap get-katex get-ace

get-bootstrap:
	curl -L https://github.com/twbs/bootstrap/releases/download/v3.3.7/bootstrap-3.3.7-dist.zip -o bootstrap.zip
	unzip bootstrap.zip
	mv bootstrap-3.3.7-dist/css/bootstrap.css css/
	mv bootstrap-3.3.7-dist/js/bootstrap.js js/
	mkdir fonts
	mv bootstrap-3.3.7-dist/fonts/* fonts/
	rm -rf bootstrap*

get-katex:
	curl -L https://code.jquery.com/jquery-3.2.0.min.js -o js/jquery-3.2.0.min.js
	curl -L https://github.com/Khan/KaTeX/releases/download/v0.7.1/katex.zip -o katex.zip
	unzip katex.zip
	mv katex imported/
	rm -rf katex*
	rm -rf bootstrap*

get-ace:
	git clone https://github.com/ajaxorg/ace-builds.git
	mv ace-builds imported/
	cp imported/mode-ocaml.js imported/ace-builds/src-noconflict/
	cp imported/theme-ggmss.js imported/ace-builds/src-noconflict/
