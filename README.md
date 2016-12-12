I. File list
------------
init.lisp                  init file that's syslinked to .stumpwmrc





II. Design
----------
A. Program design

1. Style

2. Collision handling

3. bool <function>(param& , param&) signatures

B. AVL Tree

1. Insertion
An interative insert function was written to give better performance because
recursive functions has to allocate/deallocate multiple stack frames.

2. AVLTNode
Because an AVL tree is a self-balancing tree, the node structure needs to be
extended to handle height information.


C. Splay Tree

1. Iteration vs. Recursion

2. Small functions
3. zig, zigZig, zigZag
4. hangPLeft, hangPRight 

D. Binary Heap (MaxHeap)

1. Specialization

2. Implementation


3. heapSort()


III. Analysis
-------------
The text of both authors, Bacon and Shakespeare, show a predominance of the
words "the," "of," and "and."  In Bacon's "The Essays" and "The New Atlantis,"
these common words are occurring approximately 12.4% and 14.3% of the time,
respectively.  In Shakespeare's "Hamlet" and "All's Well That Ends Well," these
words occur approximately 6.86% and 6.56%, respectively.  Based on this
evidence, we conclude that Bacon did not write Shakespeare's works.

Take that, you conspiracy theorists.



IV. Expected Bottlenecks
------------------------
A. Binary Search Tree

1. insert()/findNode()
It was hard to separate these two functions because insert() relies on
findNode() as a part of its algorithm.  This takes the longest in the BST
because it has no balancing properties.  In the case of this sorted list, it is
essentially a doubly-linked list with O(n) running time.


2. getDataAsArray()/recursiveCopy()
Stack frame allocation/deallocation from recursiveCopy() kills the performance
of these functions.


B. AVL Tree


1. insert()/findNode()
This needs to call findNode() from the BinarySearchTree class, which takes
O(log n).


C. Splay Tree


1. insert()
Similar to the AVL Tree, this needs to call findNode() from the BinarySearchTree
class, which takes O(log n).

2. splay()
This has to be called every time.  Although this is a constant time operation
for sorted input, the total running time will be linearly proportional to the
amount of data.


3. hangPRight()
This again is called every time.  It is interesting to note that this was called
twice as many times as insert(), though only one rotation is done per insert.



V. Real Bottlenecks
-------------------
A. Data weirdness

1. findNode()/std::min()
When looking at the data for the BinarySearchTree, findNode() was found to
take approximately 85% of the running time with approximately 45,000 calls.
However, std::min() makes approximately 231,000,000,000 calls but only takes
15.5% of the running time.  We believe the data is skewed/inaccurate because of
these findings.


B. Binary Search Tree

1. insert()/findNode()
As expected, these two functions took most of the processing time.

2. getDataAsArray()/recursiveCopy()
Again, as expected, these algorithms took a long time to run, although only
one call was made to getDataAsArray().


C. AVL Tree

1. insert()
This took the longest of the AVLTree functions at approximately 6.14%.


D. Splay Tree

1. insert()
Total processing time was approximately 2.11%.  


2. splay()
Total processing time was also approximately 2.11%.


3. hangPRight()
This also took approximately 2.11%.  However, it is interesting to note that
twice as many hangPRight() calls were made compared to splay().



VI. Sorting Algorithm Analysis
------------------------------

1. HeapSort - Heapsort normally runs in N log N for best, worst and average case
   It is thus reasonably efficient for binary comparisons. It should on average run
   in parallel with quicksort.

2. Selection Sort- Selection Sort is supposed to run in N^2 time for worst and best
   case scenario, and thus should be the worst of the three algorithms, except for
   extremely aberrant input.

3. QuickSort- Quicksort should run in N log N for best and average case scenarios.
   Again, it is reasonably efficient on binary comparisons and should be running
   equally well as heapsort.


In reality, heapsort is clearly the most efficient on our particular data set. We
speculate that the reason it outperforms quicksort for all the inputs is that
quicksort is not running at average time - our input is a poor represenatation of
average input for quicksort. Specifically, the input is very dense, so that many
of the different items have the same value, and thus a lot of swapping must occur,
slowing a normally fast algorithm down. It still is not obviously enough to force
quicksort to run at *worst* case time, as it still better than selection sort.
Essentially, heapsort is less affected by our semi-aberrant data than quicksort,
and selection sort is pretty bad for large data sets to begin with.

(See plots for reference)
