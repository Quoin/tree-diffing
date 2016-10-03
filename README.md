# Tree Diffing

This is a Scala library that provides a method that accepts two generic ASTs
(nodes consist of a label and value) and returns an "edit script" that lists
the steps to transform one tree to another in terms of the four operations:
insert, delete, move, and update a node.

The algorithm implemented here is also implemented [in a Java
library](https://bitbucket.org/sealuzh/tools-changedistiller) by the authors of
the Change Distilling paper (see References) with additional functionality
(e.g. source code change classification) and an Eclipse plugin.  Unfortunately,
their library is not available on the Maven Central Repository.

The algorithm is implemented using only immutable data structures.


## Usage

Perhaps the simplest way to use this is to create an implicit converter from
your domain-specific ASTs to the generic AST used by this library:

```scala
implicit def toGenericNodeTree(other: MyASTNode) =
  Node(other.someId, other.someLabel, other.someValue, other.children)
```

Ids must be unique and should be `Int`s.

If you have the implicit conversion in scope, you can just do:

```scala
import com.quoininc.treediffing.EditScript
val editOps: Seq[EditOperations] = EditScript.generate(originalTree, newTree)
```

`editOps` will be be a list of simple operations that transforms
`originalTree` into `newTree`.  See the Node object and the Chawathe paper for
more details.

## References

The tree matching algorithm comes from this paper:

B. Fluri, M. Würsch, M. Pinzger, H.C. Gall, "Change Distilling: Tree
Differencing for Fine- Grained Source Code Change Extraction", IEEE
Transactions on Software Engineering, vol. 33, no. 11, pp. 725-743, Nov. 2007

And this paper contains the edit script generator from the matched trees:

S.S. Chawathe, A. Rajaraman, H. Garcia-Molina, and J. Widom, “Change
Detection in Hierarchically Structured Information,” Proc. ACM Sigmod Int’l
Conf. Management of Data, pp. 493-504, June 1996.
