## graphql-w-persistent
#### This is a GraphQL query parser and interpreter, and it is including data processing to return the GraphQL object formats.


### News and updates

**2018-10-19 -** I appreciate the more downloads, and I also found another bug where parent entities are not returning every children data. I will look into this asap...

...Bugs are fixed. Arguments and type heirarchies are working as expected...thanks for you patience!

**2018-10-18 -** [First Hackage package release](https://hackage.haskell.org/package/graphql-w-persistent "graphql-w-persistent") is out. Querying database with multiple queries, aliases, and fragments is available while fields, linkages, and heirarchies are given to the user to define. Hopefully, more is to come!

I appreciate the many downloads, but I'm also sorry about the rough start. Since before 0.1.0.3, I've encountered several bugs and several queries were throwing errors. Some of these are now picked out, and I hope that you'll spare your patience to not turn away. I did hurry myself to release this project, but I still hope to have given a worthwhile toolkit to you...thanks for reading!

### Features

Here's a check-list from the [official documentation](https://graphql.github.io/)...

| Feature  | Present | Comments |
|----------|:-------:|----------|
| arguments to fields (optional/not and default value) | :heavy_check_mark: | 1.0.1.4 |
| data transformation arguments on scalar fields | | :thought_balloon: |
| aliases | :heavy_check_mark: | 1.0.1.0 |
| named fragments | :heavy_check_mark: | 1.0.1.0 |
| operation names | | :thought_balloon: |
| variables (default value and required/not) | | :thought_balloon: |
| directives | | |
| mutations  | | :thought_balloon: |
| inline fragments | | :thought_balloon: |
| meta fields | | |
| non-null implementation and non-null errors (on data types and arguments) | | |
| interfaces to type heirarchy | :heavy_check_mark: | 1.0.4 |
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
    stack install graphql-w-persist-0.1.0.4
    ```

#### run

```
stack runghc demonstration.hs
```

At localhost:3000, the topmost text box is the GraphQL question box. Underneath is an area to add or delete database data.

A fun query is {taxonomy{name,pet{name}}} ...enjoy!