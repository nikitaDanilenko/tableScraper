# tableScraper
A simple command line tool for extracting a specific kind of tables from an HTML document
and converting these to a .csv file.

The expected tables need to have a particular style:

* The third line of the document should contain `<meta>` information
* The table should have a header (inside a `<thead>` tag) and a body (inside a `<tbody>` tag)

All manual new lines that are created with `</br>` are transformed into slashes.

These somewhat artificial requirements originate from the use case of automatically
created tables, where the meta information is possibly corrupted,
and where each table is of said format