module dscanner.analysis.body_on_disabled_funcs;

import dscanner.analysis.base;
import dmd.astenums : STC;

extern (C++) class BodyOnDisabledFuncsCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"body_on_disabled_func_check";

	private enum string KEY = "dscanner.confusing.disabled_function_with_body";
	private enum FUNC_MSG = "Function marked with '@disabled' should not have a body";
	private enum CTOR_MSG = "Constructor marked with '@disabled' should not have a body";
	private enum DTOR_MSG = "Destructor marked with '@disabled' should not have a body";

	private bool isDisabled;

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.StorageClassDeclaration storageClassDecl)
	{
		bool wasDisabled = isDisabled;
		isDisabled = (storageClassDecl.stc & STC.disable) != 0;
		super.visit(storageClassDecl);
		isDisabled = wasDisabled;
	}

	mixin VisitAggregate!(AST.ClassDeclaration);
	mixin VisitAggregate!(AST.InterfaceDeclaration);
	mixin VisitAggregate!(AST.StructDeclaration);
	mixin VisitAggregate!(AST.UnionDeclaration);

	private template VisitAggregate(NodeType)
	{
		override void visit(NodeType node)
		{
			if (isDisabled || (node.storage_class & STC.disable) != 0)
				return;

			bool wasDisabled = isDisabled;
			isDisabled = false;
			super.visit(node);
			isDisabled = wasDisabled;
		}
	}

	mixin VisitFunction!(AST.FuncDeclaration, FUNC_MSG);
	mixin VisitFunction!(AST.CtorDeclaration, CTOR_MSG);
	mixin VisitFunction!(AST.DtorDeclaration, DTOR_MSG);

	private template VisitFunction(NodeType, string MSG)
	{
		override void visit(NodeType node)
		{
			if ((isDisabled || (node.storage_class & STC.disable) != 0) && node.fbody !is null)
				addErrorMessage(cast(ulong) node.loc.linnum, cast(ulong) node.loc.charnum, KEY, MSG);
		}
	}
}

unittest
{
	import std.stdio : stderr;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;

	StaticAnalysisConfig sac = disabledConfig();
	sac.body_on_disabled_func_check = Check.enabled;

	assertAnalyzerWarningsDMD(q{
	class C1
	{
		this() {}
		void doThing() {}
		~this() {}

		this();
		void doThing();
		~this();

	@disable:
		@disable
		{
			class UnaffectedSubClass
			{
				this() {}
				void doThing() {}
				~this() {}
			}
		}

		this() {} // [warn]: Constructor marked with '@disabled' should not have a body
		void doThing() {} // [warn]: Function marked with '@disabled' should not have a body
		~this() {} // [warn]: Destructor marked with '@disabled' should not have a body

		this();
		void doThing();
		~this();
	}

	class C2
	{
		@disable this() {} // [warn]: Constructor marked with '@disabled' should not have a body
		@disable { this() {} } // [warn]: Constructor marked with '@disabled' should not have a body
		this() @disable {} // [warn]: Constructor marked with '@disabled' should not have a body

		@disable void doThing() {} // [warn]: Function marked with '@disabled' should not have a body
		@disable doThing() {} // [warn]: Function marked with '@disabled' should not have a body
		@disable { void doThing() {} } // [warn]: Function marked with '@disabled' should not have a body
		void doThing() @disable {} // [warn]: Function marked with '@disabled' should not have a body

		@disable ~this() {} // [warn]: Destructor marked with '@disabled' should not have a body
		@disable { ~this() {} } // [warn]: Destructor marked with '@disabled' should not have a body
		~this() @disable {} // [warn]: Destructor marked with '@disabled' should not have a body

		@disable this();
		@disable { this(); }
		this() @disable;

		@disable void doThing();
		// @disable doThing(); // this is invalid grammar!
		@disable { void doThing(); }
		void doThing() @disable;

		@disable ~this();
		@disable { ~this(); }
		~this() @disable;
	}
	}c, sac);

	stderr.writeln("Unittest for BodyOnDisabledFuncsCheck passed.");
}
