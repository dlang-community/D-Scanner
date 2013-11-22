<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output indent="no" omit-xml-declaration="yes"/>
    <xsl:template match="module">
digraph {
        <xsl:for-each select="//interfaceDeclaration">
            <xsl:value-of select="./name"/>
            <xsl:value-of select="'[shape=&quot;record&quot; label=&quot;{'"/>
            <xsl:value-of select="'&lt;&lt;interface&gt;&gt;\n'"/>
            <xsl:value-of select="./name"/>
            <xsl:call-template name="bodyHandler"/>
        </xsl:for-each>    
        <xsl:for-each select="//structDeclaration">
            <xsl:value-of select="./name"/>
            <xsl:value-of select="'[shape=&quot;record&quot; label=&quot;{'"/>
            <xsl:value-of select="'&lt;&lt;struct&gt;&gt;\n'"/>
            <xsl:value-of select="./name"/>
            <xsl:call-template name="bodyHandler"/>
        </xsl:for-each>
        <xsl:for-each select="//classDeclaration">
            <xsl:value-of select="./name"/>
            <xsl:value-of select="'[shape=&quot;record&quot; label=&quot;{'"/>
            <xsl:value-of select="./name"/>
            <xsl:call-template name="bodyHandler"/>
        </xsl:for-each>
}
    </xsl:template>
    <xsl:template name="bodyHandler">
        <xsl:value-of select="'|'"/>
        <xsl:for-each select="./structBody/declaration/functionDeclaration">
            <xsl:value-of select="name"/>
            <xsl:value-of select="'('"/>
            <xsl:for-each select="./parameters/parameter">
                <xsl:value-of select="./name"/>
                <xsl:value-of select="' : '"/>
                <xsl:value-of select="./type/@pretty"/>
            </xsl:for-each>
            <xsl:value-of select="')'"/>
            <xsl:value-of select="' : '"/>
            <xsl:value-of select="./type/@pretty"/>
            <xsl:value-of select="'\l'"/>
        </xsl:for-each>
        <xsl:value-of select="'|'"/>
        <xsl:for-each select="./structBody/declaration/variableDeclaration">
            <xsl:variable name="type" select="./type/@pretty"/>
            <xsl:for-each select="./declarator">
                <xsl:value-of select="name"/>
                <xsl:value-of select="' : '"/>
                <xsl:value-of select="$type"/>
                <xsl:value-of select="'\l'"/>
            </xsl:for-each>
        </xsl:for-each>
        <xsl:value-of select="'}&quot;];'"/>
    </xsl:template>
</xsl:stylesheet>