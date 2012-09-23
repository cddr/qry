
defquery
========

Synopsis
---------

defquery is a mini-language for defining interactive database queries


Background
-----------

Any enterprise app that I've worked on has many "search" screens that
provide users with an easy way to execute canned queries against the
operational data.  I've found that the ORM frameworks out there don't
support these screens very well because they are more concerned with
creating and updating individual records and listing related records.
In contrast, "search" screens typically gather data from several
tables, filter it (sometimes using fairly complex predicates), and
present a table of results (with bonus points for exporting the
results in a variety of formats). This library provides a
mini-language to spec out these screens together with a simple web
interface.

Required 3rd Party Libraries
-----------------------------

 * Postmodern (and implicitly the PostgreSQL database) S-XML
 * hunchentoot slickgrid


API
-----

###defquery

Defines a new query named `query-name' in the global envioronment. The
body of the query consists of the following options

:predicate A boolean expression tree consisting of sub-predicates that
    may be combined using traditional primitive booleans like "and",
    "or" "<", ">" etc.  e.g. (:and (:> date-from) (:< date-thru))

    => (:and (:> date-from ?)  (:< date-thru ?))
    
    We add value over plain SQL by evaluating the predicate only once
    we've got all the input from the user so if the user does not
    specify some parameter, that sub-predicate is replaced in our
    expression by "true".

:query-view A SQL view, table, or S-SQL table expression that
            minimally provides a column for each predicate mentioned
            in the :predicate clause above.  It may provide additional
            columns but these are ignored by defquery.  It should also
            provide a `key' which we will be used to search the
            :result-view

:result-view A SQL view, table, or S-SQL table expression that
             provides the data one wishes the user to see when
             displaying the query results.

:query-ui A user-interface definition that specifies the ui controls
          necessary to elicit the query's input parameters from the
          user.  The default implementation renders these using HTML
          and javascript but there should be nothing to prevent these
          ui controls being rendered by a desktop widget library like
          LtK or GtK.

:result-ui A user-interface definition that specifies the ui
           control(s) necessary to display the query result.  The
           default implementation uses the javascript library
           slickgrid but again, other implementations should be
           possible.
