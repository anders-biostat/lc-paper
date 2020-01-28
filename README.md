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

* save html page from the browser to get a static page

* get an image with
```
wkhtmltoimage -f "svg" figA_static.html figA.svg
```