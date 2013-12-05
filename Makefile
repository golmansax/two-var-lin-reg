two_var_lin_reg_test.byte: two_var_lin_reg.ml two_var_lin_reg_test.ml
	ocamlbuild -use-ocamlfind -pkg oUnit -pkg num -I lib/* \
		two_var_lin_reg_test.byte
