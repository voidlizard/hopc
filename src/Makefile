all:
	@echo Generate BNFC stuffs
	bnfc -d -haskell -bytestring -p Compilers.Hopc.Frontend.Lisp.BNFC ./Lisp.cf
	happy -gca Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/Par.y
	alex -g Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/Lex.x
	(cd Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/; latex Doc.tex; dvips Doc.dvi -o Doc.ps)
	ghc --make Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/Test.hs -o Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/Test
clean:
	-rm -f Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/*.log Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/*.aux Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/*.hi Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/*.o Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/*.dvi
	-rm -f Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/Doc.ps
distclean: clean
	-rm -f Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/Doc.* Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/Lex.* Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/Par.* Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/Layout.* Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/Skel.* Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/Print.* Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/Test.* Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/Abs.* Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/Test Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/ErrM.* Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/SharedString.* Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/Lisp.dtd Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/XML.* Makefile*
	-rmdir -p Compilers/Hopc/Frontend/Lisp/BNFC/Lisp/
