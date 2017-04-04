from Dragon import *

script = Script("boost.dvc")

script.append(List("alnum", List.alnum))
script.append(List("digits", List.digits))

script.append(Command("include boost", ["#include <boost/>", Key.left]))

script.append(List("headers", [ "mpl",
				"fusion",
				"type_traits",
				"preprocessor"
				"numeric"
				]))
script.append(Command("include boost <headers>",
		      ["#include <boost/", Argument(0), "/>", Key.left])) 


class Mpl(dict): pass
mpl = Mpl()

mpl["classes"] = ( "vector", "list", "deque", "set", "map",
		   "range_c", "vector_c", "list_c", "set_c", "string" )

mpl["intrinsic"] = ( "at", "at_c", "back", "begin", "clear", "empty", "end",
		     "erase", "erase_key", "front", "has_key", "insert", "insert_range",
		     "is_sequence", "key_type", "order", 
		     "pop_back", "pop_front", "push_back",
		     "push_front", "sequence_tag", "size", "value_type" )

mpl["query"] = ( "find", "find_if", "contains", "count", "count_if",
		 "lower_bound", "upper_bound", "min_element", "max_element", "equal" )

mpl["transform"] = ( "copy",
		     "copy_if",
		     "transform",
		     "replace",
		     "replace_if",
		     "remove",
		     "remove_if",
		     "unique",
		     "partition",
		     "stable_partition",
		     "sort",
		     "reverse",
		     "reverse_copy",
		     "reverse_copy_if",
		     "reverse_transform",
		     "reverse_replace",
		     "reverse_replace_if",
		     "reverse_remove",
		     "reverse_remove_if",
		     "reverse_unique",
		     "reverse_partition",
		     "reverse_stable_partition" )

mpl.keyword = ( "and", "or", "int", "bool", "if", "not", "void" )

for (k,v) in mpl.items():
    script.append(List("mpl-%s" % k, [string.replace(s, "_", " ") for s in v]))
    script.append(Command("M. P. L. <mpl-%s>" % k,
			  ["mpl::", Argument(0),
			   #Function.Replace(Argument(0), " ", "-"),
			   "<>", Key.left])) 

script.append(List("mpl-bool", [ "true", "false" ]))
script.append(Command("M. P. L. <mpl-bool>", [ "mpl::", Argument(0), "_"]))

script.append(List("mpl-keyword", mpl.keyword))
script.append(Command("M. P. L. <mpl-keyword>",
		      ["mpl::", Argument(0), "_<>", Key.left])) 



script.append(List("fusion", [ "at",
			       "map",
			       "size",
			       "for_each",
			       "result_of",
			       "transform"
			       ]))
script.append(Command("fusion <fusion>",
		      ["fusion::", Argument(0), "<>", Key.left])) 


script.append(List("ublas", [ "matrix",
			      "vector",
			      "trans",
			      "range",
			      "project"
			      "matrix_expression"
			      "vector_expression"
			    ]))
script.append(Command("ublas <ublas>",
		      ["ublas::", Argument(0)])) 


print script
