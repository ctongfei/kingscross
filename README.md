### kingscross-py

Calling Python from Scala

#### Type marshalling

Kingscross marshalls/unmarshalls the following type pairs between Python and Scala:
```scala
int        <=> Int
long       <=> Long
float      <=> Float / Double
string     <=> String
tuple      <=> Tuple2[A, B] / Tuple3[A, B, C]
iterator   <=> Iterator[T]
list       <=> Seq[T]
set        <=> Set[T]
dict       <=> Map[K, V]
```
