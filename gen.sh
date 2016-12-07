function gen {
    date
    pandoc report.md --toc --template=report-tpl.tex  -o report.pdf
}
gen
while true ;do
    inotifywait -e modify report.md report-tpl.tex
    gen
done
