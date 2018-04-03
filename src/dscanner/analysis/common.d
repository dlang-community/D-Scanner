// Copyright (c) 2018, dlang-community
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module dscanner.analysis.common;

import dparse.ast;

/// Checks whether a declaration is really a declaration or just a label
bool isLabel(const Declaration decl)
{
	import std.meta : AliasSeq;
	alias properties = AliasSeq!(
		"aliasDeclaration",
		"aliasThisDeclaration",
		"anonymousEnumDeclaration",
		"attributeDeclaration",
		"classDeclaration",
		"conditionalDeclaration",
		"constructor",
		"debugSpecification",
		"destructor",
		"enumDeclaration",
		"eponymousTemplateDeclaration",
		"functionDeclaration",
		"importDeclaration",
		"interfaceDeclaration",
		"invariant_",
		"mixinDeclaration",
		"mixinTemplateDeclaration",
		"postblit",
		"pragmaDeclaration",
		"sharedStaticConstructor",
		"sharedStaticDestructor",
		"staticAssertDeclaration",
		"staticConstructor",
		"staticDestructor",
		"structDeclaration",
		"templateDeclaration",
		"unionDeclaration",
		"unittest_",
		"variableDeclaration",
		"versionSpecification",
	);
	if (decl.declarations !is null)
		return true;

	bool isNull;
	foreach (property; properties)
		if (mixin("decl." ~ property ~ " !is null"))
			isNull = true;

	return isNull;
}
