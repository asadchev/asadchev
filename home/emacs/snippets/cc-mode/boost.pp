# -*- mode: snippet -*-
# name: BOOST_PP
# key: BOOST_PP
# --
BOOST_PP_${1:$$(yas/choose-value '(
	 " "
	 "SEQ_ENUM"
	 "SEQ_FOR_EACH"
	 "SEQ_TRANSFORM"
	 "SEQ_FIRST_N"
	 "ENUM_PARAMS"
	 "ENUM_BINARY_PARAMS"
	 "INC"
	 "TUPLE_ELEM"))}
