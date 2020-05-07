#Context Validation

Goal - The goal of this library is to create a flexible data validation schema which resembles the shape of the data.

##Terminology
 - `TestRule`  - A predicate function which takes a json object and produces a passed/failed result as well as a description.
 - `ValidationReport` - The object produced by a `TestRule` which contains the passed/failed result as well as a description of the validation.
 - `DocumentParser`   - An object which takes a schema and produces a `TestRule`

##Serialization
The library use Play Json, reports can be serialized with 

```
import play.api.libs.json.Json
import com.fuego.contextvalidation.ValidationReport

val validReport = ValidationReport.passed("Sample passing Report")
Json.prettyPrint(Json.toJson(validReport.serialize))
```


##Basic Validation

####Existence
To validate the value at a path exists, simply use the `*` character.
```
{
 "v1": "*"
}
```
####Equality
The following documnet
```
{
 "v1": "test"
}
```
will successfully validate field "v1" == "test" for any input object. 
####Regex
To validate a regex at a path, simply enclose the pattern to validate in forward slashes.
```
{
 "v1": "/.*someValue/"
}
```

####Contains all strings
To validate the value at a path contains all of a list of string values, make a `contains` array for the path to validate.
```
{
 "v1": {
    "contains": ["foo", "bar"]
 }
}
```

####Contains any string
To validate the value at a path contains any of a list of string values, make an `orContains` array for the path to validate.
```
{
 "v1": {
    "orContains": ["foo", "bar"]
 }
}
```

##Combinators
A combinator takes a collection of rules and combines their result to create a single report.
Currently, the only two combinators are AND and OR, with AND being the implicit default combinator.

To combine rules with OR logic, simply do the following:
```
{
  "OR": {
     "v1": {
       "orContains": ["foo", "bar"]
     },
     "v2": {
       "orContains": ["foo", "bar"]
     }
  }
}
```
The above document will validate either field v1 or field v2 contain foo or bar. 
Once in the context of a different combinator, all nested rules will be validated with that combinator. 
To switch back to and, the change must be explicit.
```
{
  "OR": {
     "v1": {
       "orContains": ["foo", "bar"]
     },
     "AND": {
        "v2": "*",
        "v3": "*"
     }
  }
}
```
The above document will validate either v1 contains "foo" or "bar", or fields "v2" and "v3" exist.

##Nesting
The library can validate nested objects as well, for example:
```
{
  "v1": {
     "v2": "*"
  },
  "v3": "*"
}
```
##Optionality

The library can enforce optionality by appending a `?` to the end of a field.

```
{
  "v1?": "foo"
}
```
The above document will pass if field `v1` does not exist, but will fail if `v` exists and `!= "foo"`

Optionality also supports nesting.
```
{
  "v1?": {
    "v2": "*"
  }
}
```
The above document will pass if field `v1` does not exist but will fail if `v1` does exist and does not contain field `v2`

##Future Work
Support for more combinators, operators on arrays and numbers is desired. There is also likely to be edge cases and gross serialization
for mismatched shapes. This library should be considered alpha quality.

