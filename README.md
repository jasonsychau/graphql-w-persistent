## graphql-w-persistent
#### This is a GraphQL query parser and interpreter, and it is including data processing to return the GraphQL object formats.


### News and updates

**2018-11-05 -** Version 0.2.0.1 is out. We've included variables on your queries. We don't have support for not-null variables, but we'll look into it in the future. This change is an api-breaking change since we're including type-matching between variables and subfields. As a consequence, your schema is changed to include types. You can look at example on the [hackage page]() as guidance. With the next version, we will include casting, so you're results are not only text values.

Another note is of our example case were we used show on form response values. We couldn't interpret double quotes, but this is now changed to unpack. You now can use the double quotes in you queries.

Here's a question: I am now removing all \n\r linebreak characters from the query since the textarea form field is putting these into the return value. Is removing all these a problem to anyone and their data or arguments?

**2018-10-26 -** Version 0.1.0.7 is including schema set-up with json file. This is via the new function processQueryStringWithJson (Yes, I'm supposed to make version 0.1.1.0. I think that it's okay.). You can read about the json file format on the [module page](http://hackage.haskell.org/package/graphql-w-persistent-0.1.0.7/docs/GraphQL.html "GraphQL module"). The idea is to make more straightforward of detailing your schema.

**2018-10-22 -** I owe all my 103 downloaders on my first week a big apology. I gave you all incorrect instructions on giving your schema: on the fifth list where relationship tables are given, the first two tuple strings are database table names. They are not ServerObject strings nor pseudonym strings. In addition, the first list is supposed to store pseudonyms from being nested object fields. You can check the updated Hackage information to know more. I'm sorry (somebody can maybe find a QA programming internship to learn how can I test my code?)!

On another note, I've added an attribute that I didn't initially list. I've added the revision and error throwing of duplicated queries. Duplicate queries are now supposedly collapsed to a single query with union fields. I appreciate your support.

All these are in the version 0.1.0.5.

**2018-10-19 -** I appreciate the more downloads, and I also found another bug where parent entities are not returning every children data. I will look into this asap...

...Bugs are fixed. Arguments and type heirarchies are working as expected...thanks for you patience!

**2018-10-18 -** [First Hackage package release](https://hackage.haskell.org/package/graphql-w-persistent "graphql-w-persistent") is out. Querying database with multiple queries, aliases, and fragments is available while fields, linkages, and heirarchies are given to the user to define. Hopefully, more is to come!

I appreciate the many downloads, but I'm also sorry about the rough start. Since before 0.1.0.3, I've encountered several bugs and several queries were throwing errors. Some of these are now picked out, and I hope that you'll spare your patience to not turn away. I did hurry myself to release this project, but I still hope to have given a worthwhile toolkit to you...thanks for reading!

### Features

Here's a check-list from the [official documentation](https://graphql.github.io/)...

| Feature  | Present | Comments |
|----------|:-------:|----------|
| argument to root objects | :heavy_check_mark: | 0.1.0.4 |
| data transformation arguments on scalar fields | | :thought_balloon: |
| aliases | :heavy_check_mark: | 1.0.1.0 |
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
    stack install graphql-w-persist-0.2.0.1
    ```

#### run

```
stack runghc demonstration.hs
```

At localhost:3000, the topmost text box is the GraphQL question box while below is the variables textbox. Underneath is an area to add or delete database data.

A fun query is {taxonomy{name,pet{name}}} ...enjoy!