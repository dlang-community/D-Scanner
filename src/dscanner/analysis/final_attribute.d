//          Copyright Basile Burg 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.final_attribute;

import dscanner.analysis.base;
import dmd.astcodegen;
import dmd.dsymbol;
import dmd.tokens : TOK;
import std.string : format;

/**
 * Checks for useless usage of the final attribute.
 *
 * There are several cases where the compiler allows them even if it's a noop.
 */
extern (C++) class FinalAttributeChecker(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"final_attribute_check";

	enum Parent
	{
		module_,
		struct_,
		union_,
		class_,
		function_,
		interface_
	}

	bool _private;
	bool _inFinalClass;
	bool _alwaysStatic;
	bool _blockStatic;
	bool _blockFinal;
	Parent _parent = Parent.module_;

	enum pushPopPrivate = q{
		const bool wasPrivate = _private;
		_private = false;
		scope (exit) _private = wasPrivate;
	};

	extern(D) this(string fileName)
	{
		super(fileName);
	}

	override void visit(AST.StorageClassDeclaration scd)
	{
		import dmd.astenums : STC;

		if (scd.stc & STC.static_)
			_blockStatic = true;

		scope (exit) _blockStatic = false;

		if (scd.stc & STC.final_)
			_blockFinal = true;

		scope (exit) _blockFinal = false;

		if (!scd.decl)
			return;

		foreach (member; *scd.decl)
		{
			auto sd = member.isStructDeclaration();
			auto ud = member.isUnionDeclaration();

			const finalTokenOffset = (scd.stc & STC.final_) ? ownFinalOffset(member.loc.fileOffset) : size_t.max;
			if (finalTokenOffset != size_t.max)
			{

				AutoFix fix = AutoFix.replacement(finalTokenOffset, finalTokenOffset + 6, "", "Remove final attribute");

				if (!ud && sd)
					addErrorMessage([finalTokenOffset, finalTokenOffset + 5], KEY, MSGB.format(FinalAttributeChecker.MESSAGE.struct_i), [fix]);

				if (ud)
					addErrorMessage([finalTokenOffset, finalTokenOffset + 5], KEY, MSGB.format(FinalAttributeChecker.MESSAGE.union_i), [fix]);
			}

			member.accept(this);
		}
	}

	override void visit(AST.TemplateDeclaration td)
	{
		import dmd.astenums : STC;

		if (!td.members)
			return;

		foreach (member; *td.members)
        {
			auto fd = member.isFuncDeclaration();

			const finalTokenOffset = fd && (fd.storage_class & STC.final_) ? ownFinalOffset(fd.loc.fileOffset) : size_t.max;
			if (finalTokenOffset != size_t.max)
			{

				AutoFix fix = AutoFix.replacement(finalTokenOffset, finalTokenOffset + 6, "", "Remove final attribute");

				if (_parent == Parent.class_)
					addErrorMessage([finalTokenOffset, finalTokenOffset + 5], KEY, MSGB.format(FinalAttributeChecker.MESSAGE.class_t), [fix]);

				if (_parent == Parent.interface_)
					addErrorMessage([finalTokenOffset, finalTokenOffset + 5], KEY, MSGB.format(FinalAttributeChecker.MESSAGE.interface_t), [fix]);
			}
		}

	}

	override void visit(AST.ClassDeclaration cd)
	{
		if (_blockFinal && !_inFinalClass)
			_inFinalClass = true;
		else if (_inFinalClass)
			_inFinalClass = false;
		_blockStatic = false;

		mixin (pushPopPrivate);
		const Parent saved = _parent;
		_parent = Parent.class_;
		super.visit(cd);
		_parent = saved;
		_inFinalClass = false;
	}

	/**
	 * Offset of the `final` written on the declaration at `offset` itself, or
	 * `size_t.max` when it comes from an enclosing `final:` or `final { }`.
	 */
	extern (D) private size_t ownFinalOffset(size_t offset)
	{
		import dscanner.analysis.source_range : declarationStart, tokensFrom;

		foreach (token; tokensFrom(source, declarationStart(source, offset)))
		{
			if (token.start >= offset)
				break;
			if (token.value == TOK.final_)
				return token.start;
		}
		return size_t.max;
	}

	override void visit(AST.FuncDeclaration fd)
	{
		import dmd.astenums : STC;

		const finalTokenOffset = (fd.storage_class & STC.final_) ? ownFinalOffset(fd.loc.fileOffset) : size_t.max;
		if (finalTokenOffset != size_t.max)
		{

			AutoFix fix = AutoFix.replacement(finalTokenOffset, finalTokenOffset + 6, "", "Remove final attribute");

			if (_parent == Parent.class_ && _private)
				addErrorMessage([finalTokenOffset, finalTokenOffset + 5], KEY, MSGB.format(FinalAttributeChecker.MESSAGE.class_p), [fix]);
			else if (fd.storage_class & STC.static_ || _blockStatic)
				addErrorMessage([finalTokenOffset, finalTokenOffset + 5], KEY, MSGB.format(FinalAttributeChecker.MESSAGE.class_s), [fix]);
			else if (_parent == Parent.class_ && _inFinalClass)
					addErrorMessage([finalTokenOffset, finalTokenOffset + 5], KEY, MSGB.format(FinalAttributeChecker.MESSAGE.class_f), [fix]);

			if (_parent == Parent.struct_)
				addErrorMessage([finalTokenOffset, finalTokenOffset + 5], KEY, MSGB.format(FinalAttributeChecker.MESSAGE.struct_f), [fix]);

			if (_parent == Parent.union_)
				addErrorMessage([finalTokenOffset, finalTokenOffset + 5], KEY, MSGB.format(FinalAttributeChecker.MESSAGE.union_f), [fix]);

			if (_parent == Parent.module_)
				addErrorMessage([finalTokenOffset, finalTokenOffset + 5], KEY, MSGB.format(FinalAttributeChecker.MESSAGE.func_g), [fix]);

			if (_parent == Parent.function_)
				addErrorMessage([finalTokenOffset, finalTokenOffset + 5], KEY, MSGB.format(FinalAttributeChecker.MESSAGE.func_n), [fix]);
		}

		_blockStatic = false;
		mixin (pushPopPrivate);
		const Parent saved = _parent;
		_parent = Parent.function_;
		super.visit(fd);
		_parent = saved;
	}

	override void visit(AST.InterfaceDeclaration id)
	{
		_blockStatic = false;
		mixin (pushPopPrivate);
		const Parent saved = _parent;
		_parent = Parent.interface_;
		super.visit(id);
		_parent = saved;
	}

	override void visit(AST.UnionDeclaration ud)
	{
		_blockStatic = false;
		mixin (pushPopPrivate);
		const Parent saved = _parent;
		_parent = Parent.union_;
		super.visit(ud);
		_parent = saved;
	}

	override void visit(AST.StructDeclaration sd)
	{
		_blockStatic = false;
		mixin (pushPopPrivate);
		const Parent saved = _parent;
		_parent = Parent.struct_;
		super.visit(sd);
		_parent = saved;
	}

	override void visit(AST.VisibilityDeclaration vd)
	{
		if (vd.visibility.kind == Visibility.Kind.private_)
			_private = true;
		else
			_private = false;
		
		super.visit(vd);
			_private = false;
	}

	enum KEY = "dscanner.useless.final";
	enum string MSGB = "Useless final attribute, %s";
	extern(D) static struct MESSAGE
	{
		static immutable struct_i    = "structs cannot be subclassed";
		static immutable union_i     = "unions cannot be subclassed";
		static immutable class_t     = "templated functions declared within a class are never virtual";
		static immutable class_p     = "private functions declared within a class are never virtual";
		static immutable class_f     = "functions declared within a final class are never virtual";
		static immutable class_s     = "static functions are never virtual";
		static immutable interface_t = "templated functions declared within an interface are never virtual";
		static immutable struct_f    = "functions declared within a struct are never virtual";
		static immutable union_f     = "functions declared within an union are never virtual";
		static immutable func_n      = "nested functions are never virtual";
		static immutable func_g      = "global functions are never virtual";
	}
}

@system unittest
{
	import dscanner.analysis.config : Check, disabledConfig, StaticAnalysisConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD, assertAutoFix;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.final_attribute_check = Check.enabled;
	
	assertAnalyzerWarningsDMD(q{
		void foo(){}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		void foo(){void foo(){}}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		struct S{}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		union U{}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		class Foo{public final void foo(){}}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		final class Foo{static struct Bar{}}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		class Foo{private: public final void foo(){}}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		class Foo{private: public: final void foo(){}}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		class Foo{private: public: final void foo(){}}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		class Impl
		{
			private:
			static if (true)
			{
				protected final void _wrap_getSource() {}
			}
		}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		mixin template Impl()
		{
			protected final void mixin_template_can() {}
		}
	}, sac);

	// fail

	assertAnalyzerWarningsDMD(q{
		final void foo(){} /+
		^^^^^ [warn]: %s +/
	}c.format(
		(FinalAttributeChecker!ASTCodegen).MSGB.format((FinalAttributeChecker!ASTCodegen).MESSAGE.func_g)
	), sac);

	assertAnalyzerWarningsDMD(q{
		void foo(){final void foo(){}} /+
		           ^^^^^ [warn]: %s +/
	}c.format(
		(FinalAttributeChecker!ASTCodegen).MSGB.format((FinalAttributeChecker!ASTCodegen).MESSAGE.func_n)
	), sac);

	assertAnalyzerWarningsDMD(q{
		void foo()
		{
			static if (true)
			final class A{ private: final protected void foo(){}} /+
			                        ^^^^^ [warn]: %s +/
		}
	}c.format(
		(FinalAttributeChecker!ASTCodegen).MSGB.format((FinalAttributeChecker!ASTCodegen).MESSAGE.class_f)
	), sac);

	assertAnalyzerWarningsDMD(q{
		final struct Foo{} /+
		^^^^^ [warn]: %s +/
	}c.format(
		(FinalAttributeChecker!ASTCodegen).MSGB.format((FinalAttributeChecker!ASTCodegen).MESSAGE.struct_i)
	), sac);

	assertAnalyzerWarningsDMD(q{
		final union Foo{} /+
		^^^^^ [warn]: %s +/
	}c.format(
		(FinalAttributeChecker!ASTCodegen).MSGB.format((FinalAttributeChecker!ASTCodegen).MESSAGE.union_i)
	), sac);

	assertAnalyzerWarningsDMD(q{
		class Foo{private final void foo(){}} /+
		                  ^^^^^ [warn]: %s +/
	}c.format(
		(FinalAttributeChecker!ASTCodegen).MSGB.format((FinalAttributeChecker!ASTCodegen).MESSAGE.class_p)
	), sac);

	assertAnalyzerWarningsDMD(q{
		class Foo{private: final void foo(){}} /+
		                   ^^^^^ [warn]: %s +/
	}c.format(
		(FinalAttributeChecker!ASTCodegen).MSGB.format((FinalAttributeChecker!ASTCodegen).MESSAGE.class_p)
	), sac);

	assertAnalyzerWarningsDMD(q{
		interface Foo{final void foo(T)(){}} /+
		              ^^^^^ [warn]: %s +/
	}c.format(
		(FinalAttributeChecker!ASTCodegen).MSGB.format((FinalAttributeChecker!ASTCodegen).MESSAGE.interface_t)
	), sac);

	assertAnalyzerWarningsDMD(q{
		final class Foo{final void foo(){}} /+
		                ^^^^^ [warn]: %s +/
	}c.format(
		(FinalAttributeChecker!ASTCodegen).MSGB.format((FinalAttributeChecker!ASTCodegen).MESSAGE.class_f)
	), sac);

	assertAnalyzerWarningsDMD(q{
		private: final class Foo {public: private final void foo(){}} /+
		                                          ^^^^^ [warn]: %s +/
	}c.format(
		(FinalAttributeChecker!ASTCodegen).MSGB.format((FinalAttributeChecker!ASTCodegen).MESSAGE.class_p)
	), sac);

	assertAnalyzerWarningsDMD(q{
		class Foo {final static void foo(){}} /+
		           ^^^^^ [warn]: %s +/
	}c.format(
		(FinalAttributeChecker!ASTCodegen).MSGB.format((FinalAttributeChecker!ASTCodegen).MESSAGE.class_s)
	), sac);

	assertAnalyzerWarningsDMD(q{
		class Foo
		{
			void foo(){}
			static: final void foo(){} /+
			        ^^^^^ [warn]: %s +/
		}
	}c.format(
		(FinalAttributeChecker!ASTCodegen).MSGB.format((FinalAttributeChecker!ASTCodegen).MESSAGE.class_s)
	), sac);

	assertAnalyzerWarningsDMD(q{
		class Foo
		{
			void foo(){}
			static{ final void foo(){}} /+
			        ^^^^^ [warn]: %s +/
			void foo(){}
		}
	}c.format(
		(FinalAttributeChecker!ASTCodegen).MSGB.format((FinalAttributeChecker!ASTCodegen).MESSAGE.class_s)
	), sac);


	assertAnalyzerWarningsDMD(q{
		class Statement
		{
			final class UsesEH{}
			final void comeFrom(){}
		}
	}, sac);

	assertAutoFix(q{
		final void foo(){} // fix
		void foo(){final void foo(){}} // fix
		void foo()
		{
			static if (true)
			final class A{ private: final protected void foo(){}} // fix
		}
		final struct Foo{} // fix
		final union Foo{} // fix
		class Foo{private final void foo(){}} // fix
		class Foo{private: final void foo(){}} // fix
		interface Foo{final void foo(T)(){}} // fix
		final class Foo{final void foo(){}} // fix
		private: final class Foo {public: private final void foo(){}} // fix
		class Foo {final static void foo(){}} // fix
		class Foo
		{
			void foo(){}
			static: final void foo(){} // fix
		}
		class Foo
		{
			void foo(){}
			static{final void foo(){}} // fix
			void foo(){}
		}
	}, q{
		void foo(){} // fix
		void foo(){void foo(){}} // fix
		void foo()
		{
			static if (true)
			final class A{ private: protected void foo(){}} // fix
		}
		struct Foo{} // fix
		union Foo{} // fix
		class Foo{private void foo(){}} // fix
		class Foo{private: void foo(){}} // fix
		interface Foo{void foo(T)(){}} // fix
		final class Foo{void foo(){}} // fix
		private: final class Foo {public: private void foo(){}} // fix
		class Foo {static void foo(){}} // fix
		class Foo
		{
			void foo(){}
			static: void foo(){} // fix
		}
		class Foo
		{
			void foo(){}
			static{void foo(){}} // fix
			void foo(){}
		}
	}, sac);

	// final written on another line, or applied by a label or block
	assertAnalyzerWarningsDMD(q{
		final
		class C1 {}
		class C2
		{
			final
			void f() {}
		}
		struct S1
		{
			final:
			void f() {}
		}
		final { struct S2 {} }
		final:
		struct S3 {}
	}, sac);

	stderr.writeln("Unittest for FinalAttributeChecker passed.");
}
