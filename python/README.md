<img align="right" src="https://img0.etsystatic.com/151/0/9612812/il_570xN.1146611060_c3p5.jpg" height="100px" style="padding-left: 20px"/>

## kingscross-python

Calling Python from Scala through Jep

### Installation
Add
```scala
"me.tongfei" %% "kingscross-python" % "0.1.0-SNAPSHOT"
```
and build [Jep](https://github.com/mrj0/jep), the underlying bridge between JVM and Python. Add the generated `jar` to the dependencies and the generated `jnilib`/`so` to the property `java.library.path` (`-Djava.library.path=/path/to/the/generated/jnilib`).

Import 
```scala
val py = new kingscross.Python()
import py._
```
to start using `kingscross-python`.

### Overview
`kingscross` revolves around two classes: `py.Expr` and `py.Ref`. 

`py.Expr` represents a Python expression in Scala: it can be created in the following ways:
```scala
val e = py.newExpr("Python expression") // constructor
val e = py"""Python expression""" // interpolator
```
An expression is not executed in the Python interpreter: it is merely a string saved in JVM. To run this, do one of the following:
```scala
e.! // runs the expression in Python and discard its return value
val o = e.!! // runs the expression and assign the return value to an object in Python. 
// "o" is a handle to the Python object (typed as "py.Ref").
```
Both `py.Expr` and `py.Ref` (itself is a subtype of `py.Expr`) are both descendants of `scala.Dynamic`: you can call arbitrary functions on them and they'll be delegated to Python.

### Features

#### Python string interpolator `py`

```scala
val n = py"1 + 2" // n is a Python expression, typed as "py.Expr"
py"""
  for x in range($n):
  print(x)
""".! // .! executes a Python expression. Notice the interpolated $n
```
Output:
```
0
1
2
```

#### Data interchanging between Python and Scala: Type marshalling

Kingscross marshalls/unmarshalls the following type pairs between Python and Scala. 

| Scala type                       | marshalled Python type  | unmarshalled Scala type               |
|----------------------------------|-------------------------|---------------------------------------|
| `scala.Int                      `| `int                   `| `scala.Int                           `|
| `scala.Long                     `| `long                  `| `scala.Long                          `|
| `scala.Double` / `scala.Float   `| `double                `| `scala.Double` / `scala.Float        `|
| `scala.Boolean                  `| `bool                  `| `scala.Boolean                       `|
| `scala.Char                     `| `str                   `| `                                    `|
| `java.lang.CharSequence         `| `str                   `| `java.lang.String (scala.String)     `|
| `scala.Product1[A]              `| `tuple                 `| `scala.Tuple1[A]                     `|
| `scala.Product2[A, B]           `| `tuple                 `| `(A, B)                              `|
| `scala.Product3[A, B, C]        `| `tuple                 `| `(A, B, C)                           `|
| `() => A                        `| `function              `| `() => A                             `|
| `(A) => B                       `| `function              `| `(A) => B                            `|
| `(A, B) => C                    `| `function              `| `(A, B) => C                         `|
| `(A, B, C) => D                 `| `function              `| `(A, B, C) => D                      `|
| `scala.AnyRef                   `| `object                `| `py.Ref                              `|
|                                  | `iterator              `| `py.PyIterator[A]                    `|
| `scala.collection.Seq[A]        `| `list                  `| `py.PyList[A]                        `|
| `scala.collection.Set[A]        `| `set                   `| `py.PySet[A]                         `|
| `scala.collection.Map[A, B]     `| `dict                  `| `py.PyDict[A, B]                     `|
