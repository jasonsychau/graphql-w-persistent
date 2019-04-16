## graphql-w-persistent
#### This is a GraphQL query parser and interpreter, and it is including data processing to return the GraphQL object formats.

### News Posts

**2018-11-17 -** Version 0.3.1.1 is fixing data processing. Where data was before not aggregated when making relation from object to a nested object that is of generalized database entity, it is now correctly aggregated. You should be able to now freely make relations between any actually related entities of your database. All previous package versions are now deprecated.

Exceptions are now made available to help you in catching errors.

**2018-11-06 -** Version 0.3.0.0 is here to save the day. Version 0.2.whatever is bugged. We could not make nested objects in queries. Newer version is fixing this bug. API is broken (in a good way for this time) by the processing data functions where you now pass your scalar field schemas. Another API change is the scalar field schema arguments are given with the types. This is explained in the last news post. Another API change is the returned values are cast if they are Int, Rational (as Decimal), or Double. This is shown in the json return string.

Older news posts are moved to another file to not clutter this page.

Another section is going to list the stable releases where major changes are observed...

There's one more thing to mention. The hackage page is not listing all errors on processing data function. All errors are:

* InvalidObjectException (when server data is misinterpreted or an unrecognized server object is met)
* InvalidScalarException (when server data is misinterpreted)
* EOFDataProcessingException (when the given data is shorter than expected in reference to the given serve objects)
* InvalidArgumentException (when there is an internal argument error - you should not observe this)
* InvalidVariableTypeException (when an unrecognized base data type is met)
* InvalidObjectScalarFieldException (when an unrecognized object-scalar pair is met)

I'm going to try to release new packages when I need since I'm putting-out too many...thanks anyway.

**2018-11-05 -** ~~Version 0.2.0.1 is out. We've included variables on your queries. We don't have support for not-null variables, but we'll look into it in the future. This change is an api-breaking change since we're including type-matching between variables and subfields. As a consequence, your schema is changed to include types. You can look at example on the [hackage page](http://hackage.haskell.org/package/graphql-w-persistent) as guidance. With the next version, we will include casting, so you're results are not only text values.~~

Another note is of our example case were we used show on form response values. We couldn't interpret double quotes, but this is now changed to unpack. You now can use the double quotes in you queries.

Here's a question: I am now removing all \n\r linebreak characters from the query since the textarea form field is putting these into the return value. Is removing all these a problem to anyone and their data or arguments?

**2018-10-26 -** Version 0.1.0.7 is including schema set-up with json file. This is via the new function processQueryStringWithJson (Yes, I'm supposed to make version 0.1.1.0. I think that it's okay.). You can read about the json file format on the [module page](http://hackage.haskell.org/package/graphql-w-persistent-0.1.0.7/docs/GraphQL.html "GraphQL module"). The idea is to make more straightforward of detailing your schema.

**2018-10-22 -** I owe a big apology to all my 103 downloaders on my first week. I gave you all incorrect instructions on giving your schema: on the fifth list where relationship tables are given, the first two tuple strings are database table names. They are not ServerObject strings nor pseudonym strings. In addition, the first list is supposed to store pseudonyms from being nested object fields. You can check the updated Hackage information to know more. I'm sorry (somebody can maybe find a QA programming internship to learn how can I test my code?)!

On another note, I've added an attribute that I didn't initially list. I've added the revision and error throwing of duplicated queries. Duplicate queries are now supposedly collapsed to a single query with union fields. I appreciate your support.

All these are in the version 0.1.0.5.

##### Older news posts are found on [this page](https://github.com/jasonsychau/graphql-w-persistent/blob/master/oldnews.md).

### Stable Releases

0.3.1.1

### Features

Here's a check-list from the [official documentation](https://graphql.github.io/)...

| Feature  | Present | Comments |
|----------|:-------:|----------|
| argument to root objects | :heavy_check_mark: | 0.1.0.4 |
| data transformation arguments on scalar fields | | :thought_balloon: |
| aliases | :heavy_check_mark: | 0.1.0.1 |
| named fragments | :heavy_check_mark: | 0.1.0.1 |
| operation names | | :thought_balloon: |
| variables (default value ~~and required/not~~) | :heavy_check_mark: | 0.2.0.0 |
| directives | | |
| mutations  | | :thought_balloon: |
| inline fragments | | :thought_balloon: |
| meta fields | | |
| non-null implementation and non-null errors (on data types and arguments) | | |
| interfaces to type heirarchy | :heavy_check_mark: | 0.1.0.4 |
| input types | | |
| introspection | | |
| pagination | | :thought_balloon: |
| authorization moderated fields and null types (authorization layer before GraphQL) | | |
| pagination for long lists (as transformations) | | |
| server-defined fields (like counts/previews from the thinking in graphs page) and user-designed schema | | |
| business logic layer | | |
| caching | | |

### Example

#### set-up

1. make sure that you have [Stack program and compilers](https://haskell-lang.org/get-started).

2. install package (there maybe is a later version)

    ```
    stack install graphql-w-persist-0.3.1.1
    ```

#### run

```
stack runghc demonstration.hs
```

At localhost:3000, the topmost text box is the GraphQL question box while below is the variables textbox. Underneath is an area to add or delete database data.

A fun query is {pet{name,taxonomy{name}}} ...enjoy!