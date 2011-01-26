

zebra.optdate zebra.trans_opt_date zebra.err zebra.c_date zebra.s_date zebra.pic_s_date zebra.il_date zebra.java_date : zebra.m \
	builtin.int \
	io.int \
	list.int \
	maybe.int \
	private_builtin.int \
	assoc_list.int2 \
	bitmap.int2 \
	bool.int2 \
	char.int2 \
	construct.int2 \
	deconstruct.int2 \
	enum.int2 \
	map.int2 \
	ops.int2 \
	pair.int2 \
	pretty_printer.int2 \
	rtti_implementation.int2 \
	set.int2 \
	set_ordlist.int2 \
	stream.int2 \
	string.int2 \
	term.int2 \
	time.int2 \
	tree234.int2 \
	type_desc.int2 \
	univ.int2

zebra.pic_o zebra.$O : \
	assoc_list.mih \
	bitmap.mih \
	bool.mih \
	builtin.mih \
	char.mih \
	construct.mih \
	deconstruct.mih \
	enum.mih \
	io.mih \
	list.mih \
	map.mih \
	maybe.mih \
	ops.mih \
	pair.mih \
	pretty_printer.mih \
	private_builtin.mih \
	rtti_implementation.mih \
	set.mih \
	set_ordlist.mih \
	stream.mih \
	string.mih \
	term.mih \
	time.mih \
	tree234.mih \
	type_desc.mih \
	univ.mih

ifeq ($(TARGET_ASM),yes)
zebra.mh zebra.mih : zebra.s
else
zebra.mh zebra.mih : zebra.c
endif

ifeq ($(TARGET_ASM),yes)
zebra.module_dep : zebra.s
else
 ifeq ($(findstring il,$(GRADE)),il)
zebra.module_dep : zebra.il
 else
  ifeq ($(findstring java,$(GRADE)),java)
zebra.module_dep : jmercury\zebra.java
  else
zebra.module_dep : zebra.c
  endif
 endif
endif

zebra.date zebra.date0 : zebra.m \
	builtin.int3 \
	io.int3 \
	list.int3 \
	maybe.int3 \
	private_builtin.int3 \
	assoc_list.int3 \
	bitmap.int3 \
	bool.int3 \
	char.int3 \
	construct.int3 \
	deconstruct.int3 \
	enum.int3 \
	map.int3 \
	ops.int3 \
	pair.int3 \
	pretty_printer.int3 \
	rtti_implementation.int3 \
	set.int3 \
	set_ordlist.int3 \
	stream.int3 \
	string.int3 \
	term.int3 \
	time.int3 \
	tree234.int3 \
	type_desc.int3 \
	univ.int3

zebra.date0 : zebra.m \
	builtin.int3 \
	io.int3 \
	list.int3 \
	maybe.int3 \
	private_builtin.int3 \
	assoc_list.int3 \
	bitmap.int3 \
	bool.int3 \
	char.int3 \
	construct.int3 \
	deconstruct.int3 \
	enum.int3 \
	map.int3 \
	ops.int3 \
	pair.int3 \
	pretty_printer.int3 \
	rtti_implementation.int3 \
	set.int3 \
	set_ordlist.int3 \
	stream.int3 \
	string.int3 \
	term.int3 \
	time.int3 \
	tree234.int3 \
	type_desc.int3 \
	univ.int3



zebra.$O :  \
	time.mh \
	time.mh \
	bitmap.mh \
	bitmap.mh \
	string.mh \
	io.mh \
	io.mh



zebra.pic_o :  \
	time.mh \
	time.mh \
	bitmap.mh \
	bitmap.mh \
	string.mh \
	io.mh \
	io.mh


zebra.int0 : zebra.date0
	@:
zebra.int : zebra.date
	@:
zebra.int2 : zebra.date
	@:
zebra.int3 : zebra.date3
	@:
zebra.opt : zebra.optdate
	@:
zebra.trans_opt : zebra.trans_opt_date
	@:
