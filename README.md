### kingscross-py

Calling Python from Scala

<img src="https://img0.etsystatic.com/151/0/9612812/il_570xN.1146611060_c3p5.jpg" width="200px"/>

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
