### kingscross-py

Calling Python from Scala

<img src="https://img0.etsystatic.com/151/0/9612812/il_570xN.1146611060_c3p5.jpg" width="200px"/>

#### Type marshalling

Kingscross marshalls/unmarshalls the following type pairs between Python and Scala. 
Whenever a Scala type `S1` can be marshalled into Python type `P` and then unmarshall back to Scala type `S2`,
`S1 >: S2` always holds, i.e., The unmarshalled type is a special case of the marshallable types. 

| Scala type                      | marshalled Python type | unmarshalled Scala type               |
|---------------------------------|------------------------|---------------------------------------|
| scala.Int                       | int                    | scala.Int                             |
| scala.Long                      | long                   | scala.Long                            |
| scala.Double                    | double                 | scala.Double                          |
| scala.Float                     | float                  | scala.Float                           |
| scala.Boolean                   | bool                   | scala.Boolean                         |
| java.lang.CharSequence          | string                 | java.lang.String (scala.String)       |
| scala.Product2[A, B]            | tuple                  | scala.Tuple2[A, B]                    |
| scala.Product3[A, B, C]         | tuple                  | scala.Tuple3[A, B, C]                 |
|                                 | iterator               | kingscross.py.typed.Iterator[A]       |
| scala.collection.Seq[A]         | list                   | kingscross.py.typed.List[A]           |
| scala.collection.Set[A]         | set                    | kingscross.py.typed.Set[A]            |
| scala.collection.Map[A, B]      | dict                   | kingscross.py.typed.Dict[A, B]        |