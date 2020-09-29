# lc-paper

## minted

To use `minted` one needs to install `pygments`

```
sudo apt-get install python-pygments
```

and add `--shell-escape` flag for `pdflatex`.

For TeXstudio:

Options > Configure TeXstudio > Commands > PdfLaTeX
```
pdflatex -synctex=1 -interaction=nonstopmode --shell-escape %.tex
```

## Figures

Figures are made as JS/LinkedCharts scripts. To get .svg files (or may be .png)

* install wkhtmltopdf from https://wkhtmltopdf.org/index.html

* save html page from the browser to get a static page (if opened it will show both static and interactive plots, but that's fine).

* get an image with (looks like convertion to png istead of svg works way better)
```
wkhtmltoimage --enable-local-file-access -f png figA_static.html figA.png
```