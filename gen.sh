function gen {
    date
    pandoc report.md \
	   --smart \
	   --template=report-tpl.tex \
	   -V geometry:a4paper \
	   --standalone \
	   --number-sections \
	   --default-image-extension=pdf \
	   --toc \
	   --highlight-style=tango  -o report.pdf
}
gen
while true ;do
    inotifywait -e modify report.md report-tpl.tex
    gen
done
