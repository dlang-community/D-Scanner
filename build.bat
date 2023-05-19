@echo off
setlocal enabledelayedexpansion

if "%DC%"=="" set DC="dmd"
if "%DC%"=="ldc2" set DC="ldmd2"
if "%MFLAGS%"=="" set MFLAGS="-m32"

:: git might not be installed, so we provide 0.0.0 as a fallback or use
:: the existing githash file if existent
if not exist "bin" mkdir bin
git describe --tags > bin\githash_.txt
for /f %%i in ("bin\githash_.txt") do set githashsize=%%~zi
if %githashsize% == 0 (
	if not exist "bin\githash.txt" (
		echo v0.0.0 > bin\githash.txt
	)
) else (
	move /y bin\githash_.txt bin\githash.txt
)

set DFLAGS=-O -release -version=StdLoggerDisableWarning -version=CallbackAPI -version=DMDLIB -version=MARS -Jbin -Jdmd -Jdmd\compiler\src\dmd\res %MFLAGS%
set TESTFLAGS=-g -w -version=StdLoggerDisableWarning -version=CallbackAPI -version=DMDLIB -version=MARS -Jbin -Jdmd -Jdmd\compiler\src\dmd\res
set CORE=
set LIBDPARSE=
set STD=
set ANALYSIS=
set INIFILED=
set DSYMBOL=
set CONTAINERS=
set LIBDDOC=

SET DMD_FRONTEND_SRC=objc_glue.obj clone.obj transitivevisitor.obj iasm.obj iasmdmd.obj canthrow.obj tokens.obj optimize.obj func.obj semantic2.obj dvarstats.obj ph2.obj code.obj cdef.obj xmm.obj out.obj elfobj.obj glocal.obj dvec.obj code_x86.obj iasm2.obj string2.obj file2.obj obj.obj go.obj inliner.obj cc.obj bcomplex.obj mscoffobj.obj ptrntab.obj dlist.obj pdata.obj fp.obj cod3.obj os.obj cgelem.obj dcode.obj disasm86.obj exh.obj blockopt.obj aarray.obj cg.obj newman.obj dwarfdbginf.obj codebuilder.obj var.obj cod2.obj machobj.obj cgobj.obj cod4.obj dtype.obj cv4.obj backend.obj el.obj cgcod.obj cv8.obj dwarf.obj evalu8.obj ty.obj mem.obj cgxmm.obj gdag.obj gother.obj goh.obj cgcv.obj debugprint.obj cgsched.obj dwarfeh.obj cgreg.obj backconfig.obj gloop.obj divcoeff.obj cod5.obj dwarf2.obj cg87.obj nteh.obj dcgcv.obj util2.obj compress.obj type.obj elpicpie.obj gsroa.obj cgcs.obj ee.obj symbol.obj barray.obj melf.obj oper.obj cgcse.obj rtlsym.obj mscoff.obj drtlsym.obj symtab.obj dt.obj mach.obj cod1.obj global.obj filespec.obj gflow.obj elem.obj cgen.obj md5.obj chkformat.obj argtypes_sysv_x64.obj sideeffect.obj denum.obj apply.obj e2ir.obj typinf.obj statement.obj arraytypes.obj blockexit.obj init.obj scanomf.obj utils.obj parsetimevisitor.obj errorsink.obj scanmscoff.obj initsem.obj arrayop.obj nogc.obj dsymbol.obj hdrgen.obj dmangle.obj astenums.obj libmscoff.obj compiler.obj foreachvar.obj scanmach.obj dcast.obj tocsym.obj tocvdebug.obj semantic3.obj builtin.obj sapply.obj printast.obj dtemplate.obj importc.obj file_manager.obj dclass.obj argtypes_x86.obj glue.obj statement_rewrite_walker.obj target.obj aggregate.obj stringtable.obj ctfloat.obj response.obj strtold.obj port.obj aav.obj env.obj optional.obj filename.obj man.obj rootobject.obj complex.obj hash.obj region.obj utf.obj speller.obj rmem.obj array.obj longdouble.obj bitarray.obj eh.obj strictvisitor.obj permissivevisitor.obj lambdacomp.obj ctfeexpr.obj cparse.obj imphint.obj delegatize.obj access.obj identifier.obj todt.obj dmsc.obj entity.obj impcnvtab.obj dimport.obj lexer.obj dinifile.obj libomf.obj vsoptions.obj dstruct.obj aliasthis.obj ctorflow.obj errors.obj astcodegen.obj mtype.obj dtoh.obj argtypes_aarch64.obj cpreprocess.obj dmdparams.obj lib.obj id.obj parse.obj doc.obj scanelf.obj iasmgcc.obj cppmanglewin.obj stmtstate.obj ob.obj expression.obj declaration.obj location.obj dinterpret.obj inline.obj bitfields.obj string.obj int128.obj file.obj outbuffer.obj nspace.obj gluelayer.obj json.obj toir.obj intrange.obj cond.obj constfold.obj dversion.obj staticassert.obj dmodule.obj traits.obj opover.obj link.obj toctype.obj staticcond.obj statementsem.obj globals.obj libmach.obj toobj.obj s2ir.obj inlinecost.obj objc.obj visitor.obj asttypename.obj mustuse.obj dsymbolsem.obj frontend.obj safe.obj dscope.obj attrib.obj ast_node.obj escape.obj cli.obj templateparamsem.obj libelf.obj console.obj cppmangle.obj astbase.obj dmacro.obj typesem.obj expressionsem.obj

set DMD_ROOT_SRC=
for %%x in (dmd\compiler\src\dmd\common\*.d) do set DMD_ROOT_SRC=!DMD_ROOT_SRC! %%x
for %%x in (dmd\compiler\src\dmd\root\*.d) do set DMD_ROOT_SRC=!DMD_ROOT_SRC! %%x

set DMD_LEXER_SRC=^
	dmd\compiler\src\dmd\console.d ^
	dmd\compiler\src\dmd\entity.d ^
	dmd\compiler\src\dmd\errors.d ^
	dmd\compiler\src\dmd\file_manager.d ^
	dmd\compiler\src\dmd\globals.d ^
	dmd\compiler\src\dmd\id.d ^
	dmd\compiler\src\dmd\identifier.d ^
	dmd\compiler\src\dmd\lexer.d ^
	dmd\compiler\src\dmd\tokens.d ^
	dmd\compiler\src\dmd\utils.d

set DMD_PARSER_SRC=^
	dmd\compiler\src\dmd\astbase.d ^
	dmd\compiler\src\dmd\parse.d ^
	dmd\compiler\src\dmd\parsetimevisitor.d ^
	dmd\compiler\src\dmd\transitivevisitor.d ^
	dmd\compiler\src\dmd\permissivevisitor.d ^
	dmd\compiler\src\dmd\strictvisitor.d ^
	dmd\compiler\src\dmd\astenums.d

for %%x in (src\dscanner\*.d) do set CORE=!CORE! %%x
for %%x in (src\dscanner\analysis\*.d) do set ANALYSIS=!ANALYSIS! %%x
for %%x in (libdparse\src\dparse\*.d) do set LIBDPARSE=!LIBDPARSE! %%x
for %%x in (libdparse\src\std\experimental\*.d) do set LIBDPARSE=!LIBDPARSE! %%x
for %%x in (libddoc\src\ddoc\*.d) do set LIBDDOC=!LIBDDOC! %%x
for %%x in (libddoc\common\source\ddoc\*.d) do set LIBDDOC=!LIBDDOC! %%x
for %%x in (inifiled\source\*.d) do set INIFILED=!INIFILED! %%x
for %%x in (DCD\dsymbol\src\dsymbol\*.d) do set DSYMBOL=!DSYMBOL! %%x
for %%x in (DCD\dsymbol\src\dsymbol\builtin\*.d) do set DSYMBOL=!DSYMBOL! %%x
for %%x in (DCD\dsymbol\src\dsymbol\conversion\*.d) do set DSYMBOL=!DSYMBOL! %%x
for %%x in (containers\src\containers\*.d) do set CONTAINERS=!CONTAINERS! %%x
for %%x in (containers\src\containers\internal\*.d) do set CONTAINERS=!CONTAINERS! %%x

for %%x in (dmd\compiler\src\dmd\common\*.d) do %DC% %DFLAGS% -c %%x -od. -I"dmd\compiler\src"
for %%x in (dmd\compiler\src\dmd\root\*.d) do %DC% %DFLAGS% -c %%x -od. -I"dmd\compiler\src"
for %%x in (dmd\compiler\src\dmd\backend\*.d) do %DC% %DFLAGS% -c %%x -od. -I"dmd\compiler\src"
for %%x in (dmd\compiler\src\dmd\*.d) do %DC% %DFLAGS% -c %%x -od. -I"dmd\compiler\src"

%DC% %DFLAGS% -c dmd\compiler\src\dmd\backend\iasm.d -od. -ofiasm2.obj -I"dmd\compiler\src"
%DC% %DFLAGS% -c dmd\compiler\src\dmd\common\string.d -od. -ofstring2.obj -I"dmd\compiler\src"
%DC% %DFLAGS% -c dmd\compiler\src\dmd\common\file.d -od. -offile2.obj -I"dmd\compiler\src"

if "%1" == "test" goto test_cmd

@echo on
dir
echo %DMD_FRONTEND_SRC%

%DC% %MFLAGS%^
	%CORE%^
	%STD%^
	%LIBDPARSE%^
	%LIBDDOC%^
	%ANALYSIS%^
	%INIFILED%^
	%DSYMBOL%^
	%CONTAINERS%^
	%DMD_FRONTEND_SRC%^
	%DFLAGS%^
	-I"libdparse\src"^
	-I"DCD\dsymbol\src"^
	-I"containers\src"^
	-I"libddoc\src"^
	-I"libddoc\common\source"^
	-I"dmd\compiler\src"^
	-ofbin\dscanner.exe
goto eof

:test_cmd
@echo on
set TESTNAME="bin\dscanner-unittest"
%DC% %MFLAGS% ^
	%STD%^
	%LIBDPARSE%^
	%LIBDDOC%^
	%INIFILED%^
	%DSYMBOL%^
	%CONTAINERS%^
	%DMD_FRONTEND_SRC%^
	-I"libdparse\src"^
	-I"DCD\dsymbol\src"^
	-I"containers\src"^
	-I"libddoc\src"^
	-I"dmd\compiler\src"^
	-I"dmd\compiler\src\dmd\res"^
	-lib %TESTFLAGS%^
	-of%TESTNAME%.lib
if exist %TESTNAME%.lib %DC% %MFLAGS%^
	%CORE%^
	%ANALYSIS%^
	%TESTNAME%.lib^
	-I"src"^
	-I"inifiled\source"^
	-I"libdparse\src"^
	-I"DCD\dsymbol\src"^
	-I"containers\src"^
	-I"libddoc\src"^
	-I"libddoc\common\source"^
	-I"dmd\compiler\src"^
	-I"dmd\compiler\src\dmd\res"^
	-unittest^
	%TESTFLAGS%^
	-of%TESTNAME%.exe
if exist %TESTNAME%.exe %TESTNAME%.exe

if exist %TESTNAME%.obj del %TESTNAME%.obj
if exist %TESTNAME%.lib del %TESTNAME%.lib
if exist %TESTNAME%.exe del %TESTNAME%.exe

:eof
