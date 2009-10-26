<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="text" encoding="UTF-8"/>
  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- parameters -->
  <!--<xsl:param name="textattr" select="./@l1"/>-->

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- options -->
  <!--<xsl:strip-space elements="head body form entry sense add q bibl author title ref"/>-->

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- Templates: root: seek pseudo-text nodes -->
  <xsl:template match="/">
   <xsl:apply-templates select="./*"/>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- Templates: words: output TT-like format -->
  <xsl:template match="w">
    <xsl:apply-templates select="@*"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- Templates: word-attribute -->
  <xsl:template match="w/@*">
    <xsl:if test="position()>1">
      <xsl:text>&#09;</xsl:text>
    </xsl:if>
    <xsl:value-of select="name()"/>
    <xsl:text>=</xsl:text>
    <xsl:value-of select="normalize-space(.)"/>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- Templates: cooked -->
  <xsl:template match="//cooked">
    <xsl:text>&#10;</xsl:text>

    <xsl:text>%% COOKED type=</xsl:text>
    <xsl:value-of select="./@type"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:apply-templates select="./*"/>
  </xsl:template>


  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- Templates: sentence -->
  <xsl:template match="s">
    <xsl:text>&#10;</xsl:text>
    <xsl:text>%% SENTENCE&#10;</xsl:text>
    <xsl:apply-templates select="./*"/>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- Templates: default: ignore -->
  <xsl:template match="*|@*|text()|comment()|processing-instruction()" priority="-1">
    <xsl:apply-templates select="./*"/>
  </xsl:template>

</xsl:stylesheet>
