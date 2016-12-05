while true ;do
    inotifywait -e modify report.md
    date
    pandoc report.md --toc --template=report-tpl.tex -V geometry:margin=2cm   -o report.pdf
done
