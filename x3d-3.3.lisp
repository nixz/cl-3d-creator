;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; x3d-3.3.lisp --- Augmented x3d 3.3 xsd file
;;;;
;;;; Copyright (c) 2011, Nikhil Shetty <nikhil.j.shetty@gmail.com>
;;;;   All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;  o Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;;  o Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;;  o Neither the name of the author nor the names of the contributors may
;;;;    be used to endorse or promote products derived from this software
;;;;    without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;; ==========================================================================

(in-package :x3d)

(defparameter *META-SCHEMA*

<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified" attributeFormDefault="unqualified" version="3.3.2">
	<xs:include schemaLocation="x3d-3.3-Web3dExtensionsPublic.xsd"/>
	<xs:include schemaLocation="x3d-3.3-Web3dExtensionsPrivate.xsd"/>
	<xs:annotation>
		<xs:appinfo>XML Schema for the Extensible 3D (X3D) Graphics Specification tagset</xs:appinfo>
		<xs:documentation> ![CDATA[
=============================================================================

X3D Specification Schema:  	http://www.web3d.org/specifications/x3d-3.3.xsd

Schema extensions:		http://www.web3d.org/specifications/x3d-3.3-Web3dExtensionsPublic.xsd
				http://www.web3d.org/specifications/x3d-3.3-Web3dExtensionsPrivate.xsd

Web3D extensions update URL:	http://www.web3d.org/x3d/content/x3d-3.3-Web3dExtensionsPublic.xsd

Author:   Don Brutzman

Acknowledgements:
- Thanks for excellent insights and assistance from Len Bullard,
  Justin Couch, Leonard Daly, Paul Diefenbach, Rick Goldberg, Bryan Housel,
  Alan Hudson, Chris Lilley, Braden McDaniel, Tony Parisi, Nick Polys,
  Dick Puk, Jeff Sonstein, C. Michael Sperberg-McQueen, Henry Thompson
  and Joe Williams.
- Rick Goldberg of the Sun Java3D team wrote the first two versions of the
  SAI API using IDL, and provided invaluable help on the schema-based SAI.
- Joe Williams of HyperMultiMedia documented and further developed
  the scene graph interface hierarchy, making this schema possible.
- Alan Hudson added many default values and performed consistency checks.

- XML Schema validator:		http://www.w3.org/2001/03/webdata/xsv

Design summary:
- Schemas define XML tagsets in depth using a native-XML format.
- Schemas are an alternative to Document Type Definitions (DTDs).
- This X3D Schema matches the functionality of the X3D DTD and
  provides significant further capabilities, including strong type
  checking of both node and attribute values.
- An XSLT stylesheet can be applied to this schema to autogenerate
  source code for the Scene Authoring Interface (SAI), documentation
  of node lists, etc.

Design patterns:
- X3D nodes implement X3D node types and are represented by XML elements.
- X3D non-node fields are represented by XML attributes using field types.
- X3D field types are represented by XML Schema (xsd) simpleTypes.
- XML Schema list types are used to augment XML Schema simple types, to
  create arrays of X3D base types (integerList etc).
- X3D node types (X3DGroupingNode etc.) are represented by Schema complexTypes.
- X3D node types are captured as XML Schema complexType definitions.  These match
  the X3D interface hierarchy, capture strong typing of node relationships, and collect
  common attributes shared among node types.  These features also provide functional
  consistency between scene graph content and the X3D Scene Authoring Interface (SAI)
  application programming interface (API).
- XML Schema xs:group element definitions provide child-node content models,
  since complexTypes can't be used as references to unnamed element children.

X3D Schema version numbering:
- 0.4  Nodes for Core and Base profiles (20 November 2000)
- 0.5  Nodes for DIS, GeoSpatial, HumanoidAnimation, Nurbs, any others in Full profile
- 0.6  Autogenerated Scene Authoring Interface (SAI).  First show Java interfaces,
       then show multiple formats including Interface Description Language (IDL)
       and matching Java classes that implement the Java interfaces
- 0.7  Resolution of compromise/compact representations (wrapper tags), and use of
       DTD internal parameter-entity names updated to match interface hierarchy
       naming scheme
- 0.8  Exemplar content and authoring tools that correspond to X3D Schema tagset
- 2.9  Public review and Xj3D open-source implementation version
- 3.0  X3D Task Group consensus for inclusion in X3D Specification
- 3.1  X3D Specification Amendment 1
- 3.3  X3D Specification Amendment 2

=============================================================================
]] </xs:documentation>
	</xs:annotation>
	<xs:simpleType name="boundingBoxSizeType">
		<xs:annotation>
			<xs:appinfo>X3DBoundedObject indicates that bounding box values can be provided (or computed) for this node and any children.  Bounding box values approximate the volume of a containing box in the current coordinate system.  Bounding box values can optionally be provided to (or calculated by) 3D browsers.  Bounding box values are hints that can improve performance by allowing browsers to inexpensively cull geometry, thus avoiding the computational cost of trying to drawing shapes when they are outside of the current view.  boundingBoxSizeType dimensions are non-negative values.  Default value (-1 -1 -1) indicates that no bounding box size has been computed.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/group.html#Boundingboxes"/>
		</xs:annotation>
		<xs:restriction base="SFVec3f"/>
	</xs:simpleType>
	<xs:simpleType name="intensityType">
		<xs:annotation>
			<xs:appinfo>intensityType values are floats ranging from 0.0 to 1.0.</xs:appinfo>
			<xs:documentation source="_Evaluating the X3D Schema with semantic web tools_, Web3D 2012 Conference, Petit, Marc (EDF), Henry Boccon-Gibod (EDF), Mouton, Christophe (EDF)"/>
		</xs:annotation>
		<xs:restriction base="SFFloat">
			<xs:minInclusive value="0"/>
			<xs:maxInclusive value="1"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFBool">
		<xs:annotation>
			<xs:appinfo>
                SFBool is a logical type with possible values (true|false) to match the XML boolean type.
                Hint:  X3D SFBool values are lower case (true|false) in order to maintain
                compatibility with other XML documents.
            </xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFBoolAndMFBool"/>
		</xs:annotation>
		<xs:restriction base="xs:boolean"/>
	</xs:simpleType>
	<xs:simpleType name="MFBool">
		<xs:annotation>
			<xs:appinfo>
                MFBool is an array of Boolean values.
                Type MFBool was previously undefined in the VRML 97 Specification, but nevertheless needed for event utilities and scripting.
                Example use: MFBool is useful for defining a series of behavior states using a BooleanSequencer prototype.
            </xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFBoolAndMFBool"/>
		</xs:annotation>
		<xs:list itemType="xs:boolean"/>
	</xs:simpleType>
	<xs:simpleType name="SFColor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFColorAndMFColor"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))) (((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))) (((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))))?"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="MFColor">
		<xs:annotation>
			<xs:appinfo>Array values are optionally separated by commas.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFColorAndMFColor"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))) (((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))) (((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+)))( )?(,)?( )?)*"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFColorRGBA">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFColorRGBAAndMFColorRGBA"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))) (((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))) (((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))) (((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))))?"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="MFColorRGBA">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFColorRGBAAndMFColorRGBA"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))) (((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))) (((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))) (((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+)))( )?(,)?( )?)*"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFDouble">
		<xs:annotation>
			<xs:appinfo>SFDouble is a double-precision floating-point type.  Array values are optionally separated by commas.
                See GeoVRML 1.0 Recommended Practice, Section 2.3, Limitations of Single Precision for rationale.
            </xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFDoubleAndMFDouble"/>
		</xs:annotation>
		<xs:restriction base="xs:double"/>
	</xs:simpleType>
	<xs:simpleType name="MFDouble">
		<xs:annotation>
			<xs:appinfo>MFDouble is an array of Double values, i.e. a double-precision floating-point array type.
                See GeoVRML 1.0 Recommended Practice, Section 2.3, Limitations of Single Precision for rationale.
                SFDouble/MFDouble are analagous to SFDouble/MFDouble.  Array values are optionally separated by commas.
            </xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFDoubleAndMFDouble"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="(((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?)?( )?(,)?( )?)*"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFFloat">
		<xs:annotation>
			<xs:appinfo>SFFloat is a single-precision floating-point type.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFFloatAndMFFloat"/>
		</xs:annotation>
		<xs:restriction base="xs:float"/>
	</xs:simpleType>
	<xs:simpleType name="MFFloat">
		<xs:annotation>
			<xs:appinfo>MFFloat is an array of SFFloat values, i.e. a single-precision floating-point array type.  Array values are optionally separated by commas.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFFloatAndMFFloat"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="(((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?)?( )?(,)?( )?)*"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFImage">
		<xs:annotation>
			<xs:appinfo>The SFImage field specifies a single uncompressed 2-dimensional pixel image.  SFImage fields contain three integers representing the width, height and number of components in the image, followed by (width x height) hexadecimal or integer values representing the pixels in the image.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFImageAndMFImage"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:minLength value="5"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="MFImage">
		<xs:annotation>
			<xs:appinfo>MFImage is an array of SFImage values.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFImageAndMFImage"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:minLength value="5"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFInt32">
		<xs:annotation>
			<xs:appinfo>An SFInt32 field specifies one 32-bit signed integer.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFInt32AndMFInt32"/>
		</xs:annotation>
		<xs:restriction base="xs:integer"/>
	</xs:simpleType>
	<xs:simpleType name="MFInt32">
		<xs:annotation>
			<xs:appinfo>An MFInt32 field defines an array of 32-bit signed integers.  Array values are optionally separated by commas.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFInt32AndMFInt32"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((\+|\-)?(0|[1-9][0-9]*)?( )?(,)?( )?)*"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFRotation">
		<xs:annotation>
			<xs:appinfo>SFRotation is an axis-angle 4-tuple, indicating X-Y-Z direction plus angle orientation about that axis.  The first three values specify a normalized rotation axis vector about which the rotation takes place. (Thus the first three values must be within the range [-1..+1] in order to represent a normalized unit vector.  Problem:  scientific notation allows leading digit.)  The fourth value specifies the amount of right-handed rotation about that axis in radians.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFRotationAndMFRotation"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((\+|\-)?(((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))) (\+|\-)?(((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))) (\+|\-)?(((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))) (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?)?"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="MFRotation">
		<xs:annotation>
			<xs:appinfo>MFRotation is an array of SFRotation values.  Array values are optionally separated by commas.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFRotationAndMFRotation"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((\+|\-)?(((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))) (\+|\-)?(((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))) (\+|\-)?(((\.[0-9]+|0(\.[0-9]*)?)((E|e)(\+|\-)?[0-9]+)?)|(1(\.[0]*)?((E|e)\-[0-9]+)?)|([1-9](\.[0-9]*)((E|e)\-[0-9]+))) (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?( )?(,)?( )?)*"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFString">
		<xs:annotation>
			<xs:appinfo>SFString defines a single string encoded with the UTF-8 universal character set.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFStringAndMFString"/>
		</xs:annotation>
		<xs:restriction base="xs:string"/>
	</xs:simpleType>
	<xs:simpleType name="MFString">
		<xs:annotation>
			<xs:appinfo>MFString is an array of SFString values, each "quoted" and separated by whitespace.  Array values are optionally separated by commas.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFStringAndMFString"/>
		</xs:annotation>
		<xs:list itemType="xs:string"/>
	</xs:simpleType>
	<xs:simpleType name="SFTime">
		<xs:annotation>
			<xs:appinfo>The SFTime field specifies a single time value.  Time values are specified as a double-precision floating point number. Typically, SFTime fields represent the number of seconds since Jan 1, 1970, 00:00:00 GMT.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFTimeAndMFTime"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((-1(.(0)*)?)|((\+)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?))?"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="MFTime">
		<xs:annotation>
			<xs:appinfo>MFTime is an array of SFTime values.  Array values are optionally separated by commas.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFTimeAndMFTime"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((-1(.(0)*)?|(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?( )?(,)?( )?)*)?"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFVec2f">
		<xs:annotation>
			<xs:appinfo>SFVec2f is a 2-tuple pair of SFFloat values.
                Hint: SFVec2f can be used to specify a 2D single-precision coordinate.
            </xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFVec2fAndMFVec2f"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?)?"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="MFVec2f">
		<xs:annotation>
			<xs:appinfo>MFVec2f is an array of SFVec2f values.  Array values are optionally separated by commas.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFVec2fAndMFVec2f"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?( )?(,)?( )?)*"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFVec2d">
		<xs:annotation>
			<xs:appinfo>SFVec2d is a 2-tuple pair of SFDouble values.  Array values are optionally separated by commas.
                Hint: SFVec2d can be used to specify a 2D double-precision coordinate.
            </xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFVec2dAndMFVec2d"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((\+|\-)?([0-9]+(\.[0-9]*)?|\.[0-9]+)((E|e)(\+|\-)?[0-9]+)? (\+|\-)?([0-9]+(\.[0-9]*)?|\.[0-9]+)((E|e)(\+|\-)?[0-9]+)?)?"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="MFVec2d">
		<xs:annotation>
			<xs:appinfo>MFVec2d is an array of SFVec2d values.  Array values are optionally separated by commas.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFVec2dAndMFVec2d"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?( )?(,)?( )?)*"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFVec3f">
		<xs:annotation>
			<xs:appinfo>SFVec3f is a 3-tuple triplet of SFFloat values.
                Hint:  SFVec3f can be used to specify a 3D coordinate or a 3D scale value.
            </xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFVec3fAndMFVec3f"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?)?"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="MFVec3f">
		<xs:annotation>
			<xs:appinfo>MFVec3f is an array of SFVec3f values.  Array values are optionally separated by commas.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFVec3fAndMFVec3f"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?( )?(,)?( )?)*"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFVec3d">
		<xs:annotation>
			<xs:appinfo>SFVec3d is a 3-tuple triplet of SFDouble values.
                See GeoVRML 1.0 Recommended Practice, Section 2.3, Limitations of Single-Precision.
                Hint:  SFVec3d can be used to specify a georeferenced 3D coordinate.
            </xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFVec3dAndMFVec3d"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?)?"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="MFVec3d">
		<xs:annotation>
			<xs:appinfo>MFVec3d is an array of SFVec3d values.  Array values are optionally separated by commas.
                See GeoVRML 1.0 Recommended Practice, Section 2.3, Limitations of Single Precision.
                Hint:  MFVec3d can be used to specify a list of georeferenced 3D coordinates.
            </xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFVec3dAndMFVec3d"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?( )?(,)?( )?)*"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFVec4f">
		<xs:annotation>
			<xs:appinfo>SFVec4f is a 4-tuple set of single-precision floating-point values, specifying a 3D homogeneous vector.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFVec4fAndMFVec4f"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?)?"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="MFVec4f">
		<xs:annotation>
			<xs:appinfo>MFVec4f is zero or more SFVec4f values.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFVec4fAndMFVec4f"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?( )?(,)?( )?)*"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFVec4d">
		<xs:annotation>
			<xs:appinfo>SFVec4d is a 4-tuple set of double-precision floating-point values, specifying a 3D homogeneous vector.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFVec4dAndMFVec4d"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?)?"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="MFVec4d">
		<xs:annotation>
			<xs:appinfo>MFVec4d is zero or more SFVec4d values.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFVec4dAndMFVec4d"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? (\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?( )?(,)?( )?)*"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFMatrix3f">
		<xs:annotation>
			<xs:appinfo>SFMatrix3f specifies a 3x3 matrix of single-precision floating point numbers, organized in row-major fashion.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFMatrix3fAndMFMatrix3f"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="(((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? ){8}((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?))?"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="MFMatrix3f">
		<xs:annotation>
			<xs:appinfo>MFMatrix3f specifies zero or more 3x3 matrices of single-precision floating point numbers, organized in row-major fashion.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFMatrix3fAndMFMatrix3f"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="(((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? ){8}((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?)( )?(,)?( )?)*"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFMatrix3d">
		<xs:annotation>
			<xs:appinfo>SFMatrix3d specifies a 3x3 matrix of double-precision floating point numbers, organized in row-major fashion.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFMatrix3dAndMFMatrix3d"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="(((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? ){8}((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?))?"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="MFMatrix3d">
		<xs:annotation>
			<xs:appinfo>MFMatrix3d specifies zero or more 3x3 matrices of double-precision floating point numbers, organized in row-major fashion.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFMatrix3dAndMFMatrix3d"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="(((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? ){8}((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?)( )?(,)?( )?)*"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFMatrix4f">
		<xs:annotation>
			<xs:appinfo>SFMatrix4f specifies a 4x4 matrix of single-precision floating point numbers, organized in row-major fashion.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFMatrix4fAndMFMatrix4f"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="(((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? ){15}((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?))?"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="MFMatrix4f">
		<xs:annotation>
			<xs:appinfo>MFMatrix4f specifies zero or more 4x4 matrices of single-precision floating point numbers, organized in row-major fashion.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFMatrix4fAndMFMatrix4f"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="(((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? ){15}((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?)( )?(,)?( )?)*"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="SFMatrix4d">
		<xs:annotation>
			<xs:appinfo>SFMatrix4d specifies a 4x4 matrix of double-precision floating point numbers, organized in row-major fashion.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFMatrix4dAndMFMatrix4d"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="(((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? ){15}((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?))?"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="MFMatrix4d">
		<xs:annotation>
			<xs:appinfo>MFMatrix4d specifies zero or more 4x4 matrices of double-precision floating point numbers, organized in row-major fashion.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFMatrix4dAndMFMatrix4d"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:whiteSpace value="collapse"/>
			<xs:pattern value="(((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)? ){15}((\+|\-)?(0|[1-9][0-9]*)?(\.[0-9]*)?((E|e)(\+|\-)?[0-9]+)?)( )?(,)?( )?)*"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="accessTypeNames">
		<xs:annotation>
			<xs:appinfo>accessTypeNames are allowed enumeration values for accessType.  Prior names in VRML 97 were eventIn, eventOut, field, exposedField respectively.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#FieldSemantics"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:enumeration value="initializeOnly"/>
			<xs:enumeration value="inputOnly"/>
			<xs:enumeration value="outputOnly"/>
			<xs:enumeration value="inputOutput"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="ArcClose2dTypeValues">
		<xs:annotation>
			<xs:appinfo>ArcClose2dTypeValues are allowed enumeration values for ArcClose2D closureType.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry2D.html#ArcClose2D"/>
		</xs:annotation>
		<xs:restriction base="xs:token">
			<xs:enumeration value="PIE"/>
			<xs:enumeration value="CHORD"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="componentNames">
		<xs:annotation>
			<xs:appinfo>componentNames are enumeration constants used to identify the profile for each scene-graph node, and also used by X3D tag to identify the components required by the contained Scene.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#Components"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:enumeration value="Core">
				<xs:annotation>
					<xs:appinfo>Core component is provided as the basis for custom componentization.</xs:appinfo>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/core.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="CADGeometry">
				<xs:annotation>
					<xs:appinfo>CADGeometry component is provided for Computer-Aided Design (CAD) nodes..</xs:appinfo>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/CADGeometry.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="CubeMapTexturing">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/env_texture.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="DIS">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/dis.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="EnvironmentalEffects">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/enveffects.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="EnvironmentalSensor">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/envsensor.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="EventUtilities">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/utils.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Followers">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Geometry2D">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry2D.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Geometry3D">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry3D.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Geospatial">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geodata.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Grouping">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/group.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="H-Anim">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/hanim.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Interpolation">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/interp.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="KeyDeviceSensor">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/keyboard.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Layering">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/layering.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Layout">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/layout.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Lighting">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/lighting.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Navigation">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/navigation.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Networking">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/networking.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="NURBS">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="ParticleSystems">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/particle_systems.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="PickingSensor">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/picking.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="PointingDeviceSensor">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/pointingsensor.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Rendering">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="RigidBodyPhysics">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Scripting">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/scripting.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Shaders">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shaders.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Shape">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shape.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Sound">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/sound.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Text">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/text.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Texturing">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Texturing3D">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texture3D.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Time">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/time.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="VolumeRendering">
				<xs:annotation>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html"/>
				</xs:annotation>
			</xs:enumeration>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="fieldTypeName">
		<xs:annotation>
			<xs:appinfo>fieldTypeName contains the names of all X3DField types.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fieldsDef.html"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:enumeration value="SFBool"/>
			<xs:enumeration value="MFBool"/>
			<xs:enumeration value="SFColor"/>
			<xs:enumeration value="MFColor"/>
			<xs:enumeration value="SFColorRGBA"/>
			<xs:enumeration value="MFColorRGBA"/>
			<xs:enumeration value="SFDouble"/>
			<xs:enumeration value="MFDouble"/>
			<xs:enumeration value="SFFloat"/>
			<xs:enumeration value="MFFloat"/>
			<xs:enumeration value="SFImage"/>
			<xs:enumeration value="MFImage"/>
			<xs:enumeration value="SFInt32"/>
			<xs:enumeration value="SFNode"/>
			<xs:enumeration value="MFNode"/>
			<xs:enumeration value="MFInt32"/>
			<xs:enumeration value="SFRotation"/>
			<xs:enumeration value="MFRotation"/>
			<xs:enumeration value="SFString"/>
			<xs:enumeration value="MFString"/>
			<xs:enumeration value="SFTime"/>
			<xs:enumeration value="MFTime"/>
			<xs:enumeration value="SFVec2d"/>
			<xs:enumeration value="MFVec2d"/>
			<xs:enumeration value="SFVec2f"/>
			<xs:enumeration value="MFVec2f"/>
			<xs:enumeration value="SFVec3d"/>
			<xs:enumeration value="MFVec3d"/>
			<xs:enumeration value="SFVec3f"/>
			<xs:enumeration value="MFVec3f"/>
			<xs:enumeration value="SFVec4d"/>
			<xs:enumeration value="MFVec4d"/>
			<xs:enumeration value="SFVec4f"/>
			<xs:enumeration value="MFVec4f"/>
			<xs:enumeration value="SFMatrix3d"/>
			<xs:enumeration value="MFMatrix3d"/>
			<xs:enumeration value="SFMatrix3f"/>
			<xs:enumeration value="MFMatrix3f"/>
			<xs:enumeration value="SFMatrix4d"/>
			<xs:enumeration value="MFMatrix4d"/>
			<xs:enumeration value="SFMatrix4f"/>
			<xs:enumeration value="MFMatrix4f"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="fontStyleValues">
		<xs:annotation>
			<xs:appinfo>fontStyleValues are allowed enumeration values for FontStyle node type attribute.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/text.html#Fontfamilyandstyle"/>
		</xs:annotation>
		<xs:restriction base="xs:token">
			<xs:enumeration value="PLAIN"/>
			<xs:enumeration value="BOLD"/>
			<xs:enumeration value="ITALIC"/>
			<xs:enumeration value="BOLDITALIC"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="fogTypeValues">
		<xs:annotation>
			<xs:appinfo>fogTypeValues are allowed enumeration values for Fog node fogType attribute.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/enveffects.html#Fog"/>
		</xs:annotation>
		<xs:restriction base="xs:token">
			<xs:enumeration value="LINEAR"/>
			<xs:enumeration value="EXPONENTIAL"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="shaderPartTypeValues">
		<xs:annotation>
			<xs:appinfo>shaderPartTypeValues are allowed enumeration values for ShaderPart node type attribute.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shaders.html#ShaderPart"/>
		</xs:annotation>
		<xs:restriction base="xs:token">
			<xs:enumeration value="VERTEX"/>
			<xs:enumeration value="FRAGMENT"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="metaDirectionValues">
		<xs:annotation>
			<xs:appinfo>metaDirectionValues are allowed enumeration values for meta tag direction attribute.</xs:appinfo>
			<xs:documentation source="http://www.w3.org/TR/html4/struct/dirlang.html#adef-dir"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:enumeration value="rtl">
				<xs:annotation>
					<xs:appinfo>right-to-left</xs:appinfo>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="ltr">
				<xs:annotation>
					<xs:appinfo>left-to-right</xs:appinfo>
				</xs:annotation>
			</xs:enumeration>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="networkModeValues">
		<xs:annotation>
			<xs:appinfo>networkModeValues are allowed enumeration values for DIS field networkMode.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/dis.html#CommonDISfields"/>
		</xs:annotation>
		<xs:restriction base="xs:token">
			<xs:enumeration value="standAlone"/>
			<xs:enumeration value="networkReader"/>
			<xs:enumeration value="networkWriter"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="profileNames">
		<xs:annotation>
			<xs:appinfo>profileName enumeration constants are used to identify the profile for each scene-graph node, and also used by X3D tag to identify the profile of a contained Scene.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#Profiles"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:enumeration value="Core">
				<xs:annotation>
					<xs:appinfo>Core Profile includes no nodes and is provided as the basis for custom componentization.</xs:appinfo>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/coreprofile.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Interchange">
				<xs:annotation>
					<xs:appinfo>Interchange Profile equals the minimum subset of nodes needed to author lightweight compelling content.</xs:appinfo>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/interchange.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="CADInterchange">
				<xs:annotation>
					<xs:appinfo>CADInterchange Profile adds support for CADGeometry component nodes to Interchange Profile.</xs:appinfo>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/CADInterchange.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Interactive">
				<xs:annotation>
					<xs:appinfo>Interactive Profile adds interaction nodes (Anchor, KeySensor) to the minimum subset of nodes needed to author lightweight compelling content.</xs:appinfo>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/interactive.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="Immersive">
				<xs:annotation>
					<xs:appinfo>Immersive Profile equals all of the nodes in the VRML 97 Specification, plus various X3D node additions including KeySensor, StringSensor and Scene.</xs:appinfo>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/immersive.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="MPEG4Interactive"/>
			<xs:enumeration value="Full">
				<xs:annotation>
					<xs:appinfo>The Full Profile corresponds to all Immersive X3D nodes plus all approved/implemented extensions.  Full may get renamed Integrated.</xs:appinfo>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/fullProfile.html"/>
				</xs:annotation>
			</xs:enumeration>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="textureBoundaryModeValues">
		<xs:annotation>
			<xs:appinfo>textureBoundaryModeValues are allowed enumeration values for TextureProperties boundaryMode* fields.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#t-TextureBoundaryModes"/>
		</xs:annotation>
		<xs:restriction base="xs:token">
			<xs:enumeration value="CLAMP"/>
			<xs:enumeration value="CLAMP_TO_EDGE"/>
			<xs:enumeration value="CLAMP_TO_BOUNDARY"/>
			<xs:enumeration value="MIRRORED_REPEAT"/>
			<xs:enumeration value="REPEAT"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="textureMagnificationModeValues">
		<xs:annotation>
			<xs:appinfo>textureMagnificationModeValues are allowed enumeration values for TextureProperties field magnificationFilter.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#t-TextureMagnificationModes"/>
		</xs:annotation>
		<xs:restriction base="xs:token">
			<xs:enumeration value="AVG_PIXEL"/>
			<xs:enumeration value="DEFAULT"/>
			<xs:enumeration value="FASTEST"/>
			<xs:enumeration value="NEAREST_PIXEL"/>
			<xs:enumeration value="NICEST"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="textureMinificationModeValues">
		<xs:annotation>
			<xs:appinfo>textureMinificationModeValues are allowed enumeration values for TextureProperties field minificationFilter.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#t-TextureMinificationModes"/>
		</xs:annotation>
		<xs:restriction base="xs:token">
			<xs:enumeration value="AVG_PIXEL"/>
			<xs:enumeration value="AVG_PIXEL_AVG_MIPMAP"/>
			<xs:enumeration value="AVG_PIXEL_NEAREST_MIPMAP"/>
			<xs:enumeration value="DEFAULT"/>
			<xs:enumeration value="FASTEST"/>
			<xs:enumeration value="NEAREST_PIXEL"/>
			<xs:enumeration value="NEAREST_PIXEL_AVG_MIPMAP"/>
			<xs:enumeration value="NEAREST_PIXEL_NEAREST_MIPMAP"/>
			<xs:enumeration value="NICEST"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="textureCompressionModeValues">
		<xs:annotation>
			<xs:appinfo>textureCompressionModeValues are allowed enumeration values for TextureProperties field textureCompression.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#t-TextureCompressionModes"/>
		</xs:annotation>
		<xs:restriction base="xs:token">
			<xs:enumeration value="DEFAULT"/>
			<xs:enumeration value="FASTEST"/>
			<xs:enumeration value="HIGH"/>
			<xs:enumeration value="LOW"/>
			<xs:enumeration value="MEDIUM"/>
			<xs:enumeration value="NICEST"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="unitCategories">
		<xs:annotation>
			<xs:appinfo>unitValues are allowed enumeration values for standard and derived units for the UNIT statement.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#t-Standardunits"/>
		</xs:annotation>
		<xs:restriction base="xs:token">
			<xs:enumeration value="angle"/>
			<xs:enumeration value="force"/>
			<xs:enumeration value="length"/>
			<xs:enumeration value="mass"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="x3dVersion">
		<xs:annotation>
			<xs:appinfo>x3dVersion enumeration constants are used to identify the allowed versions for an X3D scene graph.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#Profiles"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:enumeration value="3.0">
				<xs:annotation>
					<xs:appinfo>X3D v3.0 approved by ISO in 2004.</xs:appinfo>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/index.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="3.1">
				<xs:annotation>
					<xs:appinfo>X3D v3.1 Amendment 1 approved by ISO in 2005.  Backwards compatibility maintained with version 3.0.</xs:appinfo>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/index.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="3.2">
				<xs:annotation>
					<xs:appinfo>X3D v3.2 Amendment 2 approved by ISO in 2007.  Backwards compatibility maintained with versions 3.0 and 3.1.</xs:appinfo>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/index.html"/>
				</xs:annotation>
			</xs:enumeration>
			<xs:enumeration value="3.3">
				<xs:annotation>
					<xs:appinfo>X3D v3.3 under review by ISO in 2012 as Draft International Standard (DIS).  Backwards compatibility maintained with versions 3.0, 3.1 and 3.2.</xs:appinfo>
					<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/X3D.html"/>
				</xs:annotation>
			</xs:enumeration>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="initializeOnlyAccessTypes">
		<xs:annotation>
			<xs:appinfo>Table of fields having accessType initializeOnly.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#FieldSemantics"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:enumeration value="bboxCenter"/>
			<xs:enumeration value="bboxSize"/>
			<xs:enumeration value="beginCap"/>
			<xs:enumeration value="bottom"/>
			<xs:enumeration value="bottomRadius"/>
			<xs:enumeration value="category"/>
			<xs:enumeration value="ccw"/>
			<xs:enumeration value="child1Url"/>
			<xs:enumeration value="child2Url"/>
			<xs:enumeration value="child3Url"/>
			<xs:enumeration value="child4Url"/>
			<xs:enumeration value="closed"/>
			<xs:enumeration value="closureType"/>
			<xs:enumeration value="colorKey"/>
			<xs:enumeration value="colorIndex"/>
			<xs:enumeration value="colorPerVertex"/>
			<xs:enumeration value="convex"/>
			<xs:enumeration value="coordIndex"/>
			<xs:enumeration value="country"/>
			<xs:enumeration value="creaseAngle"/>
			<xs:enumeration value="crossSection"/>
			<xs:enumeration value="directOutput"/>
			<xs:enumeration value="domain"/>
			<xs:enumeration value="duration"/>
			<xs:enumeration value="endCap"/>
			<xs:enumeration value="endAngle"/>
			<xs:enumeration value="extra"/>
			<xs:enumeration value="forceTransitions"/>
			<xs:enumeration value="generateMipMaps"/>
			<xs:enumeration value="geoGridOrigin"/>
			<xs:enumeration value="geometryType"/>
			<xs:enumeration value="geoSystem"/>
			<xs:enumeration value="height"/>
			<xs:enumeration value="horizontal"/>
			<xs:enumeration value="index"/>
			<xs:enumeration value="info"/>
			<xs:enumeration value="initialDestination"/>
			<xs:enumeration value="initialValue"/>
			<xs:enumeration value="innerRadius"/>
			<xs:enumeration value="internal"/>
			<xs:enumeration value="intersectionType"/>
			<xs:enumeration value="justify"/>
			<xs:enumeration value="kind"/>
			<xs:enumeration value="knot"/>
			<xs:enumeration value="language"/>
			<xs:enumeration value="leftToRight"/>
			<xs:enumeration value="lineSegments"/>
			<xs:enumeration value="mustEvaluate"/>
			<xs:enumeration value="name"/>
			<xs:enumeration value="normalIndex"/>
			<xs:enumeration value="normalPerVertex"/>
			<xs:enumeration value="numComponents"/>
			<xs:enumeration value="order"/>
			<xs:enumeration value="outerRadius"/>
			<xs:enumeration value="orientation"/>
			<xs:enumeration value="phaseFunction"/>
			<xs:enumeration value="pointSize"/>
			<xs:enumeration value="radius"/>
			<xs:enumeration value="range"/>
			<xs:enumeration value="repeatR"/>
			<xs:enumeration value="repeatS"/>
			<xs:enumeration value="repeatT"/>
			<xs:enumeration value="rootUrl"/>
			<xs:enumeration value="rotateYUp"/>
			<xs:enumeration value="rtpHeaderExpected"/>
			<xs:enumeration value="side"/>
			<xs:enumeration value="size"/>
			<xs:enumeration value="solid"/>
			<xs:enumeration value="sortOrder"/>
			<xs:enumeration value="spacing"/>
			<xs:enumeration value="spatialize"/>
			<xs:enumeration value="specific"/>
			<xs:enumeration value="speedFactor"/>
			<xs:enumeration value="spine"/>
			<xs:enumeration value="startAngle"/>
			<xs:enumeration value="style"/>
			<xs:enumeration value="subcategory"/>
			<xs:enumeration value="surfaceArea"/>
			<xs:enumeration value="texCoordIndex"/>
			<xs:enumeration value="texCoordKey"/>
			<xs:enumeration value="title"/>
			<xs:enumeration value="top"/>
			<xs:enumeration value="topToBottom"/>
			<xs:enumeration value="type"/>
			<xs:enumeration value="uClosed"/>
			<xs:enumeration value="uDimension"/>
			<xs:enumeration value="uKnot"/>
			<xs:enumeration value="uOrder"/>
			<xs:enumeration value="vClosed"/>
			<xs:enumeration value="vDimension"/>
			<xs:enumeration value="vKnot"/>
			<xs:enumeration value="vOrder"/>
			<xs:enumeration value="xDimension"/>
			<xs:enumeration value="xSpacing"/>
			<xs:enumeration value="zDimension"/>
			<xs:enumeration value="zSpacing"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="inputOnlyAccessTypes">
		<xs:annotation>
			<xs:appinfo>Table of fields having accessType inputOnly.  These are not otherwise listed in element definitions since they cannot be specified in an .x3d file.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#FieldSemantics"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:enumeration value="activate"/>
			<xs:enumeration value="set_articulationParameterValue0"/>
			<xs:enumeration value="set_articulationParameterValue1"/>
			<xs:enumeration value="set_articulationParameterValue2"/>
			<xs:enumeration value="set_articulationParameterValue3"/>
			<xs:enumeration value="set_articulationParameterValue4"/>
			<xs:enumeration value="set_articulationParameterValue5"/>
			<xs:enumeration value="set_articulationParameterValue6"/>
			<xs:enumeration value="set_articulationParameterValue7"/>
			<xs:enumeration value="set_boolean"/>
			<xs:enumeration value="set_bind"/>
			<xs:enumeration value="set_colorIndex"/>
			<xs:enumeration value="set_contacts"/>
			<xs:enumeration value="set_coordinate"/>
			<xs:enumeration value="set_coordIndex"/>
			<xs:enumeration value="set_crossSection"/>
			<xs:enumeration value="set_destination"/>
			<xs:enumeration value="set_fraction"/>
			<xs:enumeration value="set_height"/>
			<xs:enumeration value="set_index"/>
			<xs:enumeration value="set_normalIndex"/>
			<xs:enumeration value="set_orientation"/>
			<xs:enumeration value="set_position"/>
			<xs:enumeration value="set_scale"/>
			<xs:enumeration value="set_spine"/>
			<xs:enumeration value="set_texCoordIndex"/>
			<xs:enumeration value="set_triggerTime"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="outputOnlyAccessTypes">
		<xs:annotation>
			<xs:appinfo>Table of fields having accessType outputOnly.  These are not otherwise listed in element definitions since they cannot be specified in an .x3d file.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#FieldSemantics"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:enumeration value="actionKeyPress"/>
			<xs:enumeration value="actionKeyRelease"/>
			<xs:enumeration value="altKey"/>
			<xs:enumeration value="angle"/>
			<xs:enumeration value="angleRate"/>
			<xs:enumeration value="articulationParameterValue0_changed"/>
			<xs:enumeration value="articulationParameterValue1_changed"/>
			<xs:enumeration value="articulationParameterValue2_changed"/>
			<xs:enumeration value="articulationParameterValue3_changed"/>
			<xs:enumeration value="articulationParameterValue4_changed"/>
			<xs:enumeration value="articulationParameterValue5_changed"/>
			<xs:enumeration value="articulationParameterValue6_changed"/>
			<xs:enumeration value="articulationParameterValue7_changed"/>
			<xs:enumeration value="bindTime"/>
			<xs:enumeration value="body1AnchorPoint"/>
			<xs:enumeration value="body1Axis"/>
			<xs:enumeration value="body2AnchorPoint"/>
			<xs:enumeration value="body2Axis"/>
			<xs:enumeration value="centerOfRotation_changed"/>
			<xs:enumeration value="collideTime"/>
			<xs:enumeration value="controlKey"/>
			<xs:enumeration value="cycleTime"/>
			<xs:enumeration value="detonateTime"/>
			<xs:enumeration value="duration_changed"/>
			<xs:enumeration value="elapsedTime"/>
			<xs:enumeration value="enteredText"/>
			<xs:enumeration value="enterTime"/>
			<xs:enumeration value="exitTime"/>
			<xs:enumeration value="finalText"/>
			<xs:enumeration value="firedTime"/>
			<xs:enumeration value="fraction_changed"/>
			<xs:enumeration value="geovalue_changed"/>
			<xs:enumeration value="hinge1Angle"/>
			<xs:enumeration value="hinge1AngleRate"/>
			<xs:enumeration value="hinge2Angle"/>
			<xs:enumeration value="hinge2AngleRate"/>
			<xs:enumeration value="hitGeoCoord_changed"/>
			<xs:enumeration value="hitNormal_changed"/>
			<xs:enumeration value="hitPoint_changed"/>
			<xs:enumeration value="hitTexCoord_changed"/>
			<xs:enumeration value="inputFalse"/>
			<xs:enumeration value="inputNegate"/>
			<xs:enumeration value="inputTrue"/>
			<xs:enumeration value="isActive"/>
			<xs:enumeration value="isBound"/>
			<xs:enumeration value="isCollided"/>
			<xs:enumeration value="isDetonated"/>
			<xs:enumeration value="isLoaded"/>
			<xs:enumeration value="isOver"/>
			<xs:enumeration value="isPaused"/>
			<xs:enumeration value="isNetworkReader"/>
			<xs:enumeration value="isNetworkWriter"/>
			<xs:enumeration value="isRtpHeaderHeard"/>
			<xs:enumeration value="isSelected"/>
			<xs:enumeration value="isStandAlone"/>
			<xs:enumeration value="isValid"/>
			<xs:enumeration value="keyPress"/>
			<xs:enumeration value="keyRelease"/>
			<xs:enumeration value="level_changed"/>
			<xs:enumeration value="lineBounds"/>
			<xs:enumeration value="loadTime"/>
			<xs:enumeration value="modifiedFraction_changed"/>
			<xs:enumeration value="motor1Angle"/>
			<xs:enumeration value="motor1AngleRate"/>
			<xs:enumeration value="motor2Angle"/>
			<xs:enumeration value="motor2AngleRate"/>
			<xs:enumeration value="motor3Angle"/>
			<xs:enumeration value="motor3AngleRate"/>
			<xs:enumeration value="next"/>
			<xs:enumeration value="normal_changed"/>
			<xs:enumeration value="orientation_changed"/>
			<xs:enumeration value="pickedGeometry"/>
			<xs:enumeration value="pickedNormal"/>
			<xs:enumeration value="pickedPoint"/>
			<xs:enumeration value="pickedTextureCoordinate"/>
			<xs:enumeration value="position_changed"/>
			<xs:enumeration value="previous"/>
			<xs:enumeration value="progress"/>
			<xs:enumeration value="rotation_changed"/>
			<xs:enumeration value="separation"/>
			<xs:enumeration value="separationRate"/>
			<xs:enumeration value="shiftKey"/>
			<xs:enumeration value="textBounds"/>
			<xs:enumeration value="time"/>
			<xs:enumeration value="timestamp"/>
			<xs:enumeration value="touchTime"/>
			<xs:enumeration value="trackPoint_changed"/>
			<xs:enumeration value="transitionComplete"/>
			<xs:enumeration value="translation_changed"/>
			<xs:enumeration value="triggerTime"/>
			<xs:enumeration value="triggerTrue"/>
			<xs:enumeration value="triggerValue"/>
			<xs:enumeration value="value_changed"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:simpleType name="inputOutputAccessTypes">
		<xs:annotation>
			<xs:appinfo>Table of fields having accessType inputOutput.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#FieldSemantics"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:enumeration value="activeLayer"/>
			<xs:enumeration value="address"/>
			<xs:enumeration value="align"/>
			<xs:enumeration value="alpha"/>
			<xs:enumeration value="ambientIntensity"/>
			<xs:enumeration value="anchorPoint"/>
			<xs:enumeration value="angularDampingFactor"/>
			<xs:enumeration value="angularVelocity"/>
			<xs:enumeration value="anisotropicDegree"/>
			<xs:enumeration value="antennaLocation"/>
			<xs:enumeration value="applicationID"/>
			<xs:enumeration value="applied"/>
			<xs:enumeration value="appliedParameters"/>
			<xs:enumeration value="antennaPatternType"/>
			<xs:enumeration value="antennaPatternLength"/>
			<xs:enumeration value="articulationParameterArray"/>
			<xs:enumeration value="articulationParameterChangeIndicatorArray"/>
			<xs:enumeration value="articulationParameterCount"/>
			<xs:enumeration value="articulationParameterDesignatorArray"/>
			<xs:enumeration value="articulationParameterIdPartAttachedToArray"/>
			<xs:enumeration value="articulationParameterTypeArray"/>
			<xs:enumeration value="attenuation"/>
			<xs:enumeration value="autoCalc"/>
			<xs:enumeration value="autoDamp"/>
			<xs:enumeration value="autoDisable"/>
			<xs:enumeration value="autoOffset"/>
			<xs:enumeration value="avatarSize"/>
			<xs:enumeration value="axis"/>
			<xs:enumeration value="axis1"/>
			<xs:enumeration value="axis1Angle"/>
			<xs:enumeration value="axis1Torque"/>
			<xs:enumeration value="axis2"/>
			<xs:enumeration value="axis2Angle"/>
			<xs:enumeration value="axis2Torque"/>
			<xs:enumeration value="axis3Angle"/>
			<xs:enumeration value="axis3Torque"/>
			<xs:enumeration value="axisOfRotation"/>
			<xs:enumeration value="backAmbientIntensity"/>
			<xs:enumeration value="backDiffuseColor"/>
			<xs:enumeration value="backEmissiveColor"/>
			<xs:enumeration value="backShininess"/>
			<xs:enumeration value="backSpecularColor"/>
			<xs:enumeration value="backTransparency"/>
			<xs:enumeration value="backUrl"/>
			<xs:enumeration value="boundaryOpacity"/>
			<xs:enumeration value="borderColor"/>
			<xs:enumeration value="borderWidth"/>
			<xs:enumeration value="bottomUrl"/>
			<xs:enumeration value="boundaryModeS"/>
			<xs:enumeration value="boundaryModeT"/>
			<xs:enumeration value="boundaryModeR"/>
			<xs:enumeration value="beamWidth"/>
			<xs:enumeration value="bottomUrl"/>
			<xs:enumeration value="bounce"/>
			<xs:enumeration value="center"/>
			<xs:enumeration value="centerOfMass"/>
			<xs:enumeration value="centerOfRotation"/>
			<xs:enumeration value="clipBoundary"/>
			<xs:enumeration value="collisionType"/>
			<xs:enumeration value="color"/>
			<xs:enumeration value="colorSteps"/>
			<xs:enumeration value="contactNormal"/>
			<xs:enumeration value="contourStepSize"/>
			<xs:enumeration value="controlPoint"/>
			<xs:enumeration value="constantForceMix"/>
			<xs:enumeration value="contactSurfaceThickness"/>
			<xs:enumeration value="coolColor"/>
			<xs:enumeration value="createParticles"/>
			<xs:enumeration value="cryptoSystem"/>
			<xs:enumeration value="cryptoKeyID"/>
			<xs:enumeration value="cutOffAngle"/>
			<xs:enumeration value="cycleInterval"/>
			<xs:enumeration value="data"/>
			<xs:enumeration value="dataLength"/>
			<xs:enumeration value="deadReckoning"/>
			<xs:enumeration value="deletionAllowed"/>
			<xs:enumeration value="depth"/>
			<xs:enumeration value="description"/>
			<xs:enumeration value="desiredAngularVelocity1"/>
			<xs:enumeration value="desiredAngularVelocity2"/>
			<xs:enumeration value="detonationLocation"/>
			<xs:enumeration value="detonationRelativeLocation"/>
			<xs:enumeration value="detonationResult"/>
			<xs:enumeration value="diffuseColor"/>
			<xs:enumeration value="dimensions"/>
			<xs:enumeration value="direction"/>
			<xs:enumeration value="disableAngularSpeed"/>
			<xs:enumeration value="disableLinearSpeed"/>
			<xs:enumeration value="disableTime"/>
			<xs:enumeration value="diskAngle"/>
			<xs:enumeration value="displacements"/>
			<xs:enumeration value="displayed"/>
			<xs:enumeration value="easeInEaseOut"/>
			<xs:enumeration value="edgeColor"/>
			<xs:enumeration value="emissiveColor"/>
			<xs:enumeration value="enabled"/>
			<xs:enumeration value="enabledAxes"/>
			<xs:enumeration value="encodingScheme"/>
			<xs:enumeration value="entityID"/>
			<xs:enumeration value="entityKind"/>
			<xs:enumeration value="entityDomain"/>
			<xs:enumeration value="entityCountry"/>
			<xs:enumeration value="entityCategory"/>
			<xs:enumeration value="entitySubCategory"/>
			<xs:enumeration value="entitySpecific"/>
			<xs:enumeration value="entityExtra"/>
			<xs:enumeration value="errorCorrection"/>
			<xs:enumeration value="eventApplicationID"/>
			<xs:enumeration value="eventEntityID"/>
			<xs:enumeration value="eventNumber"/>
			<xs:enumeration value="eventSiteID"/>
			<xs:enumeration value="family"/>
			<xs:enumeration value="fanCount"/>
			<xs:enumeration value="fieldOfView"/>
			<xs:enumeration value="filled"/>
			<xs:enumeration value="finiteRotationAxis"/>
			<xs:enumeration value="fired1"/>
			<xs:enumeration value="fired2"/>
			<xs:enumeration value="fireMissionIndex"/>
			<xs:enumeration value="firingRange"/>
			<xs:enumeration value="firingRate"/>
			<xs:enumeration value="fixed"/>
			<xs:enumeration value="fogType"/>
			<xs:enumeration value="force"/>
			<xs:enumeration value="forceID"/>
			<xs:enumeration value="forces"/>
			<xs:enumeration value="frequency"/>
			<xs:enumeration value="frictionCoefficients"/>
			<xs:enumeration value="frictionDirection"/>
			<xs:enumeration value="frontUrl"/>
			<xs:enumeration value="function"/>
			<xs:enumeration value="fuse"/>
			<xs:enumeration value="geoCenter"/>
			<xs:enumeration value="geoCoords"/>
			<xs:enumeration value="global"/>
			<xs:enumeration value="gradientThreshold"/>
			<xs:enumeration value="gravity"/>
			<xs:enumeration value="groundAngle"/>
			<xs:enumeration value="groundColor"/>
			<xs:enumeration value="gustiness"/>
			<xs:enumeration value="hatchColor"/>
			<xs:enumeration value="hatched"/>
			<xs:enumeration value="hatchStyle"/>
			<xs:enumeration value="headlight"/>
			<xs:enumeration value="image"/>
			<xs:enumeration value="inertia"/>
			<xs:enumeration value="info"/>
			<xs:enumeration value="inputSource"/>
			<xs:enumeration value="integerKey"/>
			<xs:enumeration value="intensity"/>
			<xs:enumeration value="intensityThreshold"/>
			<xs:enumeration value="iterations"/>
			<xs:enumeration value="isPickable"/>
			<xs:enumeration value="key"/>
			<xs:enumeration value="keyVelocity"/>
			<xs:enumeration value="jump"/>
			<xs:enumeration value="keyValue"/>
			<xs:enumeration value="leftUrl"/>
			<xs:enumeration value="length"/>
			<xs:enumeration value="lengthOfModulationParameters"/>
			<xs:enumeration value="lifetimeVariation"/>
			<xs:enumeration value="lighting"/>
			<xs:enumeration value="limitOrientation"/>
			<xs:enumeration value="linearAcceleration"/>
			<xs:enumeration value="linearDampingFactor"/>
			<xs:enumeration value="linearVelocity"/>
			<xs:enumeration value="linetype"/>
			<xs:enumeration value="linewidthScaleFactor"/>
			<xs:enumeration value="llimit"/>
			<xs:enumeration value="load"/>
			<xs:enumeration value="location"/>
			<xs:enumeration value="loop"/>
			<xs:enumeration value="marking"/>
			<xs:enumeration value="mass"/>
			<xs:enumeration value="magnificationFilter"/>
			<xs:enumeration value="maxAngle"/>
			<xs:enumeration value="matrix"/>
			<xs:enumeration value="maxAngle1"/>
			<xs:enumeration value="maxBack"/>
			<xs:enumeration value="maxCorrectionSpeed"/>
			<xs:enumeration value="maxExtent"/>
			<xs:enumeration value="maxFront"/>
			<xs:enumeration value="maxParticles"/>
			<xs:enumeration value="maxPosition"/>
			<xs:enumeration value="maxSeparation"/>
			<xs:enumeration value="maxTorque1"/>
			<xs:enumeration value="maxTorque2"/>
			<xs:enumeration value="minAngle"/>
			<xs:enumeration value="minAngle1"/>
			<xs:enumeration value="minBack"/>
			<xs:enumeration value="minBounceSpeed"/>
			<xs:enumeration value="minFront"/>
			<xs:enumeration value="minificationFilter"/>
			<xs:enumeration value="minPosition"/>
			<xs:enumeration value="minSeparation"/>
			<xs:enumeration value="mode"/>
			<xs:enumeration value="modulationTypeSpreadSpectrum"/>
			<xs:enumeration value="modulationTypeMajor"/>
			<xs:enumeration value="modulationTypeDetail"/>
			<xs:enumeration value="modulationTypeSystem"/>
			<xs:enumeration value="momentsOfInertia"/>
			<xs:enumeration value="motor1Axis"/>
			<xs:enumeration value="motor2Axis"/>
			<xs:enumeration value="motor3Axis"/>
			<xs:enumeration value="multicastRelayHost"/>
			<xs:enumeration value="multicastRelayPort"/>
			<xs:enumeration value="munitionEndPoint"/>
			<xs:enumeration value="munitionStartPoint"/>
			<xs:enumeration value="munitionApplicationID"/>
			<xs:enumeration value="munitionEntityID"/>
			<xs:enumeration value="munitionSiteID"/>
			<xs:enumeration value="munitionQuantity"/>
			<xs:enumeration value="mustOutput"/>
			<xs:enumeration value="name"/>
			<xs:enumeration value="navType"/>
			<xs:enumeration value="networkMode"/>
			<xs:enumeration value="normalizeVelocity"/>
			<xs:enumeration value="objectType"/>
			<xs:enumeration value="offset"/>
			<xs:enumeration value="offsetUnits"/>
			<xs:enumeration value="on"/>
			<xs:enumeration value="opacityFactor"/>
			<xs:enumeration value="ordered"/>
			<xs:enumeration value="orthogonalColor"/>
			<xs:enumeration value="parallelColor"/>
			<xs:enumeration value="parameter"/>
			<xs:enumeration value="particleLifetime"/>
			<xs:enumeration value="particleSize"/>
			<xs:enumeration value="pauseTime"/>
			<xs:enumeration value="pickable"/>
			<xs:enumeration value="pitch"/>
			<xs:enumeration value="plane"/>
			<xs:enumeration value="point"/>
			<xs:enumeration value="port"/>
			<xs:enumeration value="position"/>
			<xs:enumeration value="power"/>
			<xs:enumeration value="preferAccuracy"/>
			<xs:enumeration value="priority"/>
			<xs:enumeration value="radioID"/>
			<xs:enumeration value="radioEntityTypeKind"/>
			<xs:enumeration value="radioEntityTypeDomain"/>
			<xs:enumeration value="radioEntityTypeCountry"/>
			<xs:enumeration value="radioEntityTypeCategory"/>
			<xs:enumeration value="radioEntityTypeNomenclature"/>
			<xs:enumeration value="radioEntityTypeNomenclatureVersion"/>
			<xs:enumeration value="readInterval"/>
			<xs:enumeration value="receivedPower"/>
			<xs:enumeration value="receiverState"/>
			<xs:enumeration value="reference"/>
			<xs:enumeration value="relativeAntennaLocation"/>
			<xs:enumeration value="retainedOpacity"/>
			<xs:enumeration value="retainUserOffsets"/>
			<xs:enumeration value="resumeTime"/>
			<xs:enumeration value="rightUrl"/>
			<xs:enumeration value="rotation"/>
			<xs:enumeration value="sampleRate"/>
			<xs:enumeration value="samples"/>
			<xs:enumeration value="scale"/>
			<xs:enumeration value="scaleMode"/>
			<xs:enumeration value="scaleOrientation"/>
			<xs:enumeration value="segmentEnabled"/>
			<xs:enumeration value="separateBackColor"/>
			<xs:enumeration value="shininess"/>
			<xs:enumeration value="shadows"/>
			<xs:enumeration value="silhouetteBoundaryOpacity"/>
			<xs:enumeration value="silhouetteRetainedOpacity"/>
			<xs:enumeration value="silhouetteSharpness"/>
			<xs:enumeration value="siteID"/>
			<xs:enumeration value="sizeUnits"/>
			<xs:enumeration value="skinCoordIndex"/>
			<xs:enumeration value="skinCoordWeight"/>
			<xs:enumeration value="skyColor"/>
			<xs:enumeration value="skyAngle"/>
			<xs:enumeration value="sliderForce"/>
			<xs:enumeration value="slipCoefficients"/>
			<xs:enumeration value="slipFactors"/>
			<xs:enumeration value="softnessConstantForceMix"/>
			<xs:enumeration value="softnessErrorCorrection"/>
			<xs:enumeration value="source"/>
			<xs:enumeration value="specularColor"/>
			<xs:enumeration value="speed"/>
			<xs:enumeration value="startTime"/>
			<xs:enumeration value="stiffness"/>
			<xs:enumeration value="stopBounce"/>
			<xs:enumeration value="stopErrorCorrection"/>
			<xs:enumeration value="stop1ErrorCorrection"/>
			<xs:enumeration value="stop1ConstantForceMix"/>
			<xs:enumeration value="stop1Bounce"/>
			<xs:enumeration value="stop2Bounce"/>
			<xs:enumeration value="stop3Bounce"/>
			<xs:enumeration value="stop1ErrorCorrection"/>
			<xs:enumeration value="stop2ErrorCorrection"/>
			<xs:enumeration value="stop3ErrorCorrection"/>
			<xs:enumeration value="stopTime"/>
			<xs:enumeration value="string"/>
			<xs:enumeration value="stripCount"/>
			<xs:enumeration value="summary"/>
			<xs:enumeration value="surfaceSpeed"/>
			<xs:enumeration value="surfaceTolerance"/>
			<xs:enumeration value="surfaceValues"/>
			<xs:enumeration value="suspensionErrorCorrection"/>
			<xs:enumeration value="suspensionForce"/>
			<xs:enumeration value="tau"/>
			<xs:enumeration value="tdlType"/>
			<xs:enumeration value="tessellation"/>
			<xs:enumeration value="tessellationScale"/>
			<xs:enumeration value="textureCompression"/>
			<xs:enumeration value="texturePriority"/>
			<xs:enumeration value="timeOut"/>
			<xs:enumeration value="toggle"/>
			<xs:enumeration value="tolerance"/>
			<xs:enumeration value="topUrl"/>
			<xs:enumeration value="torques"/>
			<xs:enumeration value="transitionTime"/>
			<xs:enumeration value="transitionType"/>
			<xs:enumeration value="translation"/>
			<xs:enumeration value="transmitFrequencyBandwidth"/>
			<xs:enumeration value="transmitState"/>
			<xs:enumeration value="transmitterApplicationID"/>
			<xs:enumeration value="transmitterEntityID"/>
			<xs:enumeration value="transmitterRadioID"/>
			<xs:enumeration value="transmitterSiteID"/>
			<xs:enumeration value="transparent"/>
			<xs:enumeration value="transparency"/>
			<xs:enumeration value="turbulence"/>
			<xs:enumeration value="type"/>
			<xs:enumeration value="ulimit"/>
			<xs:enumeration value="update"/>
			<xs:enumeration value="url"/>
			<xs:enumeration value="useFiniteRotation"/>
			<xs:enumeration value="useGeometry"/>
			<xs:enumeration value="useGlobalGravity"/>
			<xs:enumeration value="uTessellation"/>
			<xs:enumeration value="variation"/>
			<xs:enumeration value="value"/>
			<xs:enumeration value="version"/>
			<xs:enumeration value="vector"/>
			<xs:enumeration value="vertexCount"/>
			<xs:enumeration value="vertices"/>
			<xs:enumeration value="visibilityLimit"/>
			<xs:enumeration value="visibilityRange"/>
			<xs:enumeration value="visible"/>
			<xs:enumeration value="vTessellation"/>
			<xs:enumeration value="warhead"/>
			<xs:enumeration value="warmColor"/>
			<xs:enumeration value="weight"/>
			<xs:enumeration value="weightConstant1"/>
			<xs:enumeration value="weightConstant2"/>
			<xs:enumeration value="weightFunction1"/>
			<xs:enumeration value="weightFunction2"/>
			<xs:enumeration value="whichChoice"/>
			<xs:enumeration value="whichGeometry"/>
			<xs:enumeration value="writeInterval"/>
			<xs:enumeration value="yScale"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:attributeGroup name="DEF_USE">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#DEFL_USESemantics"/>
		</xs:annotation>
		<xs:attribute name="DEF" type="xs:ID">
			<xs:annotation>
				<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#DEFL_USESemantics"/>
			</xs:annotation>
		</xs:attribute>
		<xs:attribute name="USE" type="xs:IDREF">
			<xs:annotation>
				<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#DEFL_USESemantics"/>
			</xs:annotation>
		</xs:attribute>
	</xs:attributeGroup>
	<xs:attributeGroup name="globalAttributes">
		<xs:attribute name="class" type="xs:NMTOKENS">
			<xs:annotation>
				<xs:documentation source="http://www.web3d.org/files/specifications/19776-1/V3.2/Part01/concepts.html#ClassAttributeSyntax"/>
			</xs:annotation>
		</xs:attribute>
	</xs:attributeGroup>
	<xs:complexType name="X3DAppearanceNode" abstract="false" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shape.html#X3DAppearanceNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DNode">
				<xs:group ref="AppearanceChildContentModel" minOccurs="0" maxOccurs="unbounded"/>
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="appearance"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DAppearanceChildNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>Nodes of this type can be used as child nodes for Appearance.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shape.html#X3DAppearanceChildNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DNode"/>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DBackgroundNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/enveffects.html#X3DBackgroundNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DBindableNode">
				<xs:attribute name="groundAngle" type="MFFloat"/>
				<xs:attribute name="groundColor" type="MFColor"/>
				<xs:attribute name="skyAngle" type="MFFloat"/>
				<xs:attribute name="skyColor" type="MFColor" default="0 0 0"/>
				<xs:attribute name="transparency" type="SFFloat" default="0"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DBindableNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>Bindable nodes implement the binding stack, so that only one of each node type is active at a given time.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/core.html#X3DBindableNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DChildNode"/>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DBoundedObject" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>X3DBoundedObject indicates that bounding box values can
                be provided (or computed) for this node and any children.
                Bounding box values approximate the volume of a containing box
                in the current coordinate system.  Bounding box values can
                optionally be provided to (or calculated by) 3D browsers.
                Bounding box values are hints that can improve performance by
                allowing browsers to inexpensively cull geometry, thus avoiding
                the computational cost of trying to drawing shapes when they are
                outside of the current view.
            </xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/group.html#X3DBoundedObject"/>
		</xs:annotation>
		<xs:attribute name="bboxCenter" type="SFVec3f" default="0 0 0"/>
		<xs:attribute name="bboxSize" type="boundingBoxSizeType" default="-1 -1 -1"/>
	</xs:complexType>
	<xs:complexType name="X3DChildNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>A node that implements X3DChildNode is one of the legal children for a X3DGroupingNode parent.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/core.html#X3DChildNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DNode">
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="children"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DColorNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#X3DColorNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DGeometricPropertyNode">
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="color"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DComposedGeometryNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>Composed geometry nodes produce renderable geometry, can contain Color Coordinate Normal TextureCoordinate, and are contained by a Shape node.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#X3DComposedGeometryNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DGeometryNode">
				<xs:group ref="ComposedGeometryContentModel" minOccurs="0" maxOccurs="unbounded"/>
				<xs:attribute name="ccw" type="SFBool" default="true"/>
				<xs:attribute name="colorPerVertex" type="SFBool" default="true"/>
				<xs:attribute name="normalPerVertex" type="SFBool" default="true"/>
				<xs:attribute name="solid" type="SFBool" default="true"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DCoordinateNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#X3DCoordinateNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DGeometricPropertyNode"/>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DDragSensorNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/pointingsensor.html#X3DDragSensorNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DPointingDeviceSensorNode">
				<xs:attribute name="autoOffset" type="SFBool" default="true"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DEnvironmentalSensorNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/envsensor.html#X3DEnvironmentalSensorNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DSensorNode">
				<xs:attribute name="center" type="SFVec3f" default="0 0 0"/>
				<xs:attribute name="size" type="SFVec3f" default="0 0 0"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DEnvironmentTextureNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/env_texture.html#X3DEnvironmentTextureNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DAppearanceChildNode">
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="texture"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DField" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>X3DField is equivalent to SF (Single Field) simple non-Node types in the VRML 97 Specification.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/technicalinfo/specifications/vrml97/part1/fieldsRef.html#5.1.2"/>
		</xs:annotation>
	</xs:complexType>
	<xs:complexType name="X3DFogObject" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/enveffects.html#X3DFogOjbect"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DNode">
				<xs:attribute name="color" type="SFColor" default="1 1 1"/>
				<xs:attribute name="fogType" type="fogTypeValues" default="LINEAR"/>
				<xs:attribute name="visibilityRange" type="SFFloat" default="0"/>
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="children"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DFontStyleNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/text.html#X3DFontStyleNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DNode">
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="fontStyle"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DGeometryNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>Geometry nodes produce renderable geometry and are contained by a Shape node.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#X3DGeometryNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DNode">
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="geometry"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DGeometricPropertyNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>This is the base node type for all geometric property node types.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#X3DGeometricPropertyNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DNode"/>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DGroupingNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DBoundedObject"/>
                            Grouping nodes can contain other nodes as children, thus making up the backbone of a scene graph.
                        </xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/group.html#X3DGroupingNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DChildNode">
				<xs:sequence-cl minOccurs="0" maxOccurs="unbounded">
					<xs:group ref="ChildContentModel" minOccurs="0" maxOccurs="unbounded"/>
					<xs:group ref="ChildContentModelSceneGraphStructure" minOccurs="0" maxOccurs="unbounded"/>
				</xs:sequence-cl>
				<xs:attribute name="bboxCenter" type="SFVec3f" default="0 0 0"/>
				<xs:attribute name="bboxSize" type="boundingBoxSizeType" default="-1 -1 -1"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DHumanoidNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>X3DHumanoidNode enables extensibility for the Humanoid node.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/hanim.html#HAnimHumanoid"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DChildNode"/>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DInfoNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/group.html#X3DInfoNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DChildNode"/>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DInterpolatorNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>Interpolator nodes are designed for linear keyframed animation.
                Interpolators are driven by an input key ranging [0..1] and produce corresponding piecewise-linear output functions.
            </xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/interp.html#X3DInterpolatorNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DChildNode">
				<xs:attribute name="key" type="MFFloat"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DKeyDeviceSensorNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/keyboard.html#X3DKeyDeviceSensorNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DSensorNode"/>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DLightNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>Light nodes provide illumination for rendering geometry in the scene.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/lighting.html#X3DLightNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DChildNode">
				<xs:attribute name="ambientIntensity" type="intensityType" default="0"/>
				<xs:attribute name="color" type="SFColor" default="1 1 1"/>
				<xs:attribute name="intensity" type="intensityType" default="1"/>
				<xs:attribute name="on" type="SFBool" default="true"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DMaterialNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shape.html#X3DMaterialNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DAppearanceChildNode">
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="material"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DMetadataObject">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/core.html#X3DMetadataObject"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DNode">
				<xs:attribute name="name" type="SFString"/>
				<xs:attribute name="reference" type="SFString"/>
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="metadata">
					<xs:annotation>
						<xs:appinfo>containerField='value' for contained payload metadata inside MetadataSet element.</xs:appinfo>
					</xs:annotation>
				</xs:attribute>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DNetworkSensorNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/networking.html#X3DNetworkSensorNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DSensorNode"/>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>All instantiable nodes implement X3DNode, which corresponds to SFNode in the VRML 97 specification.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/core.html#X3DNode"/>
		</xs:annotation>
		<xs:sequence-cl>
			<xs:element ref="IS" minOccurs="0"/>
			<xs:group ref="ChildContentModelCore" minOccurs="0"/>
		</xs:sequence-cl>
		<xs:attributeGroup ref="DEF_USE"/>
		<xs:attributeGroup ref="globalAttributes"/>
	</xs:complexType>
	<xs:complexType name="X3DNodeMixedContent" abstract="true" mixed="true">
		<xs:annotation>
			<xs:appinfo>X3DNodeMixedContent matches X3DNode, with the exception that implementing nodes can have mixed content (i.e. both elements and plain text).</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/core.html#X3DNode"/>
		</xs:annotation>
		<xs:sequence-cl>
			<xs:element ref="IS" minOccurs="0"/>
			<xs:group ref="ChildContentModelCore" minOccurs="0"/>
		</xs:sequence-cl>
		<xs:attributeGroup ref="DEF_USE"/>
		<xs:attributeGroup ref="globalAttributes"/>
	</xs:complexType>
	<xs:complexType name="X3DNormalNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#X3DNormalNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DGeometricPropertyNode">
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="normal"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DNurbsControlCurveNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html#X3DParametricGeometryNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DNode">
				<xs:attribute name="controlPoint" type="MFVec2d"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DNurbsSurfaceGeometryNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html#X3DNurbsSurfaceGeometryNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DParametricGeometryNode">
				<xs:attribute name="uClosed" type="SFBool" default="false"/>
				<xs:attribute name="vClosed" type="SFBool" default="false"/>
				<xs:attribute name="uDimension" type="SFInt32" default="0"/>
				<xs:attribute name="vDimension" type="SFInt32" default="0"/>
				<xs:attribute name="uKnot" type="MFDouble"/>
				<xs:attribute name="vKnot" type="MFDouble"/>
				<xs:attribute name="uOrder" type="SFInt32" default="3"/>
				<xs:attribute name="vOrder" type="SFInt32" default="3"/>
				<xs:attribute name="uTessellation" type="SFInt32" default="0"/>
				<xs:attribute name="vTessellation" type="SFInt32" default="0"/>
				<xs:attribute name="weight" type="MFDouble"/>
				<xs:attribute name="solid" type="SFBool" default="true"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DParametricGeometryNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html#X3DParametricGeometryNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DGeometryNode"/>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DPointingDeviceSensorNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/pointingsensor.html#X3DPointingDeviceSensorNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DSensorNode">
				<xs:attribute name="description" type="SFString"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DProductStructureChildNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>The X3DProductStructureChildNode abstract node type marks nodes that are valid product structure children for the CADInterchange component.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/group.html#X3DBoundedObject"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DChildNode">
				<xs:attribute name="name" type="SFString"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DProgrammableShaderObject" abstract="true" mixed="true">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shaders.html#X3DProgrammableShaderObject"/>
		</xs:annotation>
		<xs:sequence-cl>
			<xs:element ref="field" minOccurs="0" maxOccurs="unbounded"/>
			<xs:element ref="IS" minOccurs="0"/>
			<xs:group ref="ChildContentModelCore" minOccurs="0"/>
		</xs:sequence-cl>
		<xs:attributeGroup ref="DEF_USE"/>
		<xs:attributeGroup ref="globalAttributes"/>
	</xs:complexType>
	<xs:complexType name="X3DPrototypeInstanceNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/core.html#X3DPrototypeInstance"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DNode"/>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DScriptNode" abstract="true" mixed="true">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/scripting.html#X3DScriptNode"/>
		</xs:annotation>
		<xs:sequence-cl>
			<xs:element ref="field" minOccurs="0" maxOccurs="unbounded"/>
			<xs:element ref="IS" minOccurs="0"/>
			<xs:group ref="ChildContentModelCore" minOccurs="0"/>
		</xs:sequence-cl>
		<xs:attributeGroup ref="DEF_USE"/>
		<xs:attributeGroup ref="globalAttributes"/>
		<xs:attribute name="containerField" type="xs:NMTOKEN" default="children"/>
	</xs:complexType>
	<xs:complexType name="X3DSensorNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/core.html#X3DSensorNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DChildNode">
				<xs:attribute name="enabled" type="SFBool" default="true"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DSequencerNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/utils.html#X3DSequencerNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DChildNode">
				<xs:attribute name="key" type="MFFloat"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DShaderNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shaders.html#X3DShaderNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DAppearanceChildNode">
				<xs:attribute name="language" type="SFString"/>
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="shaders">
					<xs:annotation>
						<xs:documentation>parent Appearance node</xs:documentation>
					</xs:annotation>
				</xs:attribute>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DShapeNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DBoundedObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shape.html#X3DShapeNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DChildNode">
				<xs:group ref="ShapeChildContentModel" minOccurs="0"/>
				<xs:attribute name="bboxCenter" type="SFVec3f" default="0 0 0"/>
				<xs:attribute name="bboxSize" type="boundingBoxSizeType" default="-1 -1 -1"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DSoundNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/sound.html#X3DSoundNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DChildNode">
				<xs:group ref="SoundChildContentModel" minOccurs="0"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DSoundSourceNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>Nodes implementing X3DSoundSourceNode are allowed children of Sound node</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/sound.html#X3DSoundSourceNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DTimeDependentNode">
				<xs:attribute name="pitch" type="SFFloat" default="1.0"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DTextureNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#X3DTextureNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DAppearanceChildNode">
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="texture"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DTexture2DNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#X3DTexture2DNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DTextureNode">
				<xs:choice minOccurs="0">
					<xs:annotation>
						<xs:documentation>textureProperties</xs:documentation>
					</xs:annotation>
					<xs:element ref="TextureProperties"/>
					<xs:element ref="ProtoInstance">
						<xs:annotation>
							<xs:documentation>Appropriately typed substitute node</xs:documentation>
						</xs:annotation>
					</xs:element>
				</xs:choice>
				<xs:attribute name="repeatS" type="SFBool" default="true"/>
				<xs:attribute name="repeatT" type="SFBool" default="true"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DTexture3DNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texture3D.html#X3DTexture3DNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DTextureNode">
				<xs:choice minOccurs="0">
					<xs:annotation>
						<xs:documentation>textureProperties</xs:documentation>
					</xs:annotation>
					<xs:element ref="TextureProperties"/>
					<xs:element ref="ProtoInstance">
						<xs:annotation>
							<xs:documentation>Appropriately typed substitute node</xs:documentation>
						</xs:annotation>
					</xs:element>
				</xs:choice>
				<xs:attribute name="repeatS" type="SFBool" default="false"/>
				<xs:attribute name="repeatT" type="SFBool" default="false"/>
				<xs:attribute name="repeatR" type="SFBool" default="false"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DTextureCoordinateNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#X3DTextureCoordinateNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DGeometricPropertyNode">
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="texCoord"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DTextureTransformNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#X3DTextureTransformNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DAppearanceChildNode">
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="textureTransform"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DTextureTransform2DNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#X3DTextureTransform2DNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DTextureTransformNode">
				<xs:attribute name="center" type="SFVec2f" default="0 0"/>
				<xs:attribute name="rotation" type="SFFloat" default="0"/>
				<xs:attribute name="scale" type="SFVec2f" default="1 1"/>
				<xs:attribute name="translation" type="SFVec2f" default="0 0"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DTimeDependentNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/time.html#X3DTimeDependentNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DChildNode">
				<xs:attribute name="loop" type="SFBool" default="false"/>
				<xs:attribute name="pauseTime" type="SFTime" default="0"/>
				<xs:attribute name="resumeTime" type="SFTime" default="0"/>
				<xs:attribute name="startTime" type="SFTime" default="0"/>
				<xs:attribute name="stopTime" type="SFTime" default="0"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DTouchSensorNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/pointingsensor.html#X3DTouchSensorNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DPointingDeviceSensorNode"/>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DTriggerNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/utils.html#X3DTriggerNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DChildNode"/>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DUrlObject" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>X3DUrlObject indicates that a node has content loaded from a URL and can be tracked via a LoadSensor.
            </xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/networking.html#X3DUrlObject"/>
		</xs:annotation>
		<xs:attribute name="url" type="MFString"/>
	</xs:complexType>
	<xs:complexType name="X3DVertexAttributeNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>
            </xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shaders.html#X3DVertexAttributeNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DGeometricPropertyNode">
				<xs:attribute name="name" type="SFString"/>
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="attrib"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DPrototype" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#PrototypeSemantics"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="SceneGraphStructureNodeType">
				<xs:attribute name="name" type="xs:NMTOKEN" use="required"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="SceneGraphStructureNodeType" abstract="false" mixed="false">
		<xs:annotation>
			<xs:appinfo>SceneGraphStructureNodeType is a marker interface that identifies nonrenderable nodes relating to scene graph structure.  SceneGraphStructureNodeType extends from Base rather than X3DNode since DEF/USE not appropriate for declarations.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#scenegraph"/>
		</xs:annotation>
	</xs:complexType>
	<xs:complexType name="WildcardNodeType" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>This complexType will likely not be needed, since integrating ProtoInstance into the content models solves the wildcard and extensibility problems.</xs:appinfo>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DNode"/>
		</xs:complexContent>
	</xs:complexType>
	<xs:group name="AppearanceChildContentModel">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DAppearanceChildNode.  Appearance can contain FillProperties, LineProperties, Material, any Texture node and any TextureTransform node, in any order. No more than one instance of these nodes is allowed.  Appearance may also contain multiple shaders (ComposedShader, PackagedShader, ProgramShader).</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shape.html#Appearancenode"/>
		</xs:annotation>
		<xs:choice>
			<xs:group ref="AppearanceChildContentModelNoProtoInstance"/>
			<xs:element ref="ProtoInstance">
				<xs:annotation>
					<xs:documentation>Appropriately typed substitute node</xs:documentation>
				</xs:annotation>
			</xs:element>
		</xs:choice>
	</xs:group>
	<xs:group name="AppearanceChildContentModelNoProtoInstance">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DAppearanceChildNode.  Appearance can contain FillProperties, LineProperties, Material, TwoSidedMaterial, any Texture node and any TextureTransform node, in any order. No more than one instance of these nodes is allowed.  Appearance may also contain multiple shaders (ComposedShader, PackagedShader, ProgramShader).</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shape.html#Appearancenode"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="FillProperties">
				<xs:annotation>
					<xs:documentation>fillProperties</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="LineProperties">
				<xs:annotation>
					<xs:documentation>lineProperties</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="Material">
				<xs:annotation>
					<xs:documentation>material</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="TwoSidedMaterial">
				<xs:annotation>
					<xs:documentation>material</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="ComposedShader">
				<xs:annotation>
					<xs:documentation>shaders</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="PackagedShader">
				<xs:annotation>
					<xs:documentation>shaders</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="ProgramShader">
				<xs:annotation>
					<xs:documentation>shaders</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="ComposedCubeMapTexture">
				<xs:annotation>
					<xs:documentation>texture</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="ComposedTexture3D">
				<xs:annotation>
					<xs:documentation>texture</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="ImageTexture">
				<xs:annotation>
					<xs:documentation>texture</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="ImageTexture3D">
				<xs:annotation>
					<xs:documentation>texture</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="MovieTexture">
				<xs:annotation>
					<xs:documentation>texture</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="MultiTexture">
				<xs:annotation>
					<xs:documentation>texture</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="PixelTexture">
				<xs:annotation>
					<xs:documentation>texture</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="PixelTexture3D">
				<xs:annotation>
					<xs:documentation>texture</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="GeneratedCubeMapTexture">
				<xs:annotation>
					<xs:documentation>texture</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="ImageCubeMapTexture">
				<xs:annotation>
					<xs:documentation>texture</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="MultiTextureTransform">
				<xs:annotation>
					<xs:documentation>textureTransform</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="TextureTransform">
				<xs:annotation>
					<xs:documentation>textureTransform</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="TextureTransform3D">
				<xs:annotation>
					<xs:documentation>textureTransform</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="TextureTransformMatrix3D">
				<xs:annotation>
					<xs:documentation>textureTransform</xs:documentation>
				</xs:annotation>
			</xs:element>
		</xs:choice>
	</xs:group>
	<xs:group name="ChildContentModel">
		<xs:annotation>
			<xs:appinfo>ChildContentModel is the child-node content model corresponding to X3DChildNode, combining all profiles.  ChildContentModel can contain most nodes, other Grouping nodes, Prototype declarations and ProtoInstances in any order and any combination. When the assigned profile is less than Full, the precise palette of legal nodes that are available depends on assigned profile and components.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/group.html#GroupingAndChildrenNodes">children</xs:documentation>
		</xs:annotation>
		<xs:choice>
			<xs:group ref="ChildContentModelInterchange"/>
			<xs:group ref="ChildContentModelInteractive"/>
			<xs:group ref="ChildContentModelImmersive"/>
			<xs:group ref="ChildContentModelFull"/>
			<xs:group ref="ChildContentModelDIS"/>
			<xs:group ref="ChildContentModelGeoSpatial"/>
			<xs:group ref="ChildContentModelHumanoidAnimation"/>
			<xs:group ref="ChildContentModelNurbs"/>
			<xs:group ref="ChildContentModelProtoInstance"/>
		</xs:choice>
	</xs:group>
	<xs:group name="ChildContentModelCore">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DChildNode for Core profile.  ChildContentModelCore enables the first child of any node to be MetadataDouble, MetadataFloat, MetadataInteger, MetadataSet or MetadataString.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/coreprofile.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="MetadataBoolean"/>
			<xs:element ref="MetadataDouble"/>
			<xs:element ref="MetadataFloat"/>
			<xs:element ref="MetadataInteger"/>
			<xs:element ref="MetadataSet"/>
			<xs:element ref="MetadataString"/>
		</xs:choice>
	</xs:group>
	<xs:group name="ChildContentModelInterchange">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DChildNode for Interchange profile.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/interchange.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="Background"/>
			<xs:element ref="ColorInterpolator"/>
			<xs:element ref="CoordinateInterpolator"/>
			<xs:element ref="DirectionalLight"/>
			<xs:element ref="Group"/>
			<xs:element ref="NavigationInfo"/>
			<xs:element ref="NormalInterpolator"/>
			<xs:element ref="OrientationInterpolator"/>
			<xs:element ref="PositionInterpolator"/>
			<xs:element ref="ScalarInterpolator"/>
			<xs:element ref="Shape"/>
			<xs:element ref="TimeSensor"/>
			<xs:element ref="Transform"/>
			<xs:element ref="Viewpoint"/>
			<xs:element ref="WorldInfo"/>
		</xs:choice>
	</xs:group>
	<xs:group name="ChildContentModelInteractive">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DChildNode for Interactive profile.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/interactive.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="Anchor"/>
			<xs:element ref="BooleanFilter"/>
			<xs:element ref="BooleanSequencer"/>
			<xs:element ref="BooleanToggle"/>
			<xs:element ref="BooleanTrigger"/>
			<xs:element ref="CylinderSensor"/>
			<xs:element ref="Inline"/>
			<xs:element ref="IntegerSequencer"/>
			<xs:element ref="IntegerTrigger"/>
			<xs:element ref="KeySensor"/>
			<xs:element ref="PlaneSensor"/>
			<xs:element ref="PointLight"/>
			<xs:element ref="ProximitySensor"/>
			<xs:element ref="SphereSensor"/>
			<xs:element ref="SpotLight"/>
			<xs:element ref="StringSensor"/>
			<xs:element ref="Switch"/>
			<xs:element ref="TimeTrigger"/>
			<xs:element ref="TouchSensor"/>
		</xs:choice>
	</xs:group>
	<xs:group name="ChildContentModelImmersive">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DChildNode for Immersive profile.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/immersive.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="AudioClip"/>
			<xs:element ref="Billboard"/>
			<xs:element ref="Collision"/>
			<xs:element ref="Fog"/>
			<xs:element ref="LoadSensor"/>
			<xs:element ref="LocalFog"/>
			<xs:element ref="LOD"/>
			<xs:element ref="Script"/>
			<xs:element ref="Sound"/>
			<xs:element ref="VisibilitySensor"/>
		</xs:choice>
	</xs:group>
	<xs:group name="ChildContentModelFull">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DChildNode for Full profile.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/Full.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="CoordinateInterpolator2D"/>
			<xs:element ref="PositionInterpolator2D"/>
			<xs:element ref="ClipPlane"/>
			<xs:element ref="EaseInEaseOut"/>
			<xs:element ref="LinePickSensor"/>
			<xs:element ref="PickableGroup"/>
			<xs:element ref="PointPickSensor"/>
			<xs:element ref="PrimitivePickSensor"/>
			<xs:element ref="VolumePickSensor"/>
			<xs:element ref="SplinePositionInterpolator"/>
			<xs:element ref="SplinePositionInterpolator2D"/>
			<xs:element ref="SplineScalarInterpolator"/>
			<xs:element ref="SquadOrientationInterpolator"/>
			<xs:element ref="StaticGroup"/>
			<xs:element ref="CADAssembly"/>
			<xs:element ref="CADLayer"/>
			<xs:element ref="OrthoViewpoint"/>
			<xs:element ref="ViewpointGroup"/>
			<xs:element ref="ColorChaser"/>
			<xs:element ref="ColorDamper"/>
			<xs:element ref="CoordinateChaser"/>
			<xs:element ref="CoordinateDamper"/>
			<xs:element ref="OrientationChaser"/>
			<xs:element ref="OrientationDamper"/>
			<xs:element ref="PositionChaser"/>
			<xs:element ref="PositionChaser2D"/>
			<xs:element ref="PositionDamper"/>
			<xs:element ref="PositionDamper2D"/>
			<xs:element ref="ScalarChaser"/>
			<xs:element ref="ScalarDamper"/>
			<xs:element ref="TexCoordChaser2D"/>
			<xs:element ref="TexCoordDamper2D"/>
			<xs:element ref="TextureBackground"/>
			<xs:element ref="CollidableShape"/>
			<xs:element ref="CollisionSensor"/>
			<xs:element ref="RigidBodyCollection"/>
			<xs:element ref="LayerSet"/>
			<xs:element ref="ParticleSystem"/>
			<xs:element ref="TransformSensor"/>
			<xs:element ref="IsoSurfaceVolumeData"/>
			<xs:element ref="SegmentedVolumeData"/>
			<xs:element ref="VolumeData"/>
		</xs:choice>
	</xs:group>
	<xs:group name="ChildContentModelDIS">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DChildNode for DIS component.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/dis.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="EspduTransform"/>
			<xs:element ref="ReceiverPdu"/>
			<xs:element ref="SignalPdu"/>
			<xs:element ref="TransmitterPdu"/>
			<xs:element ref="DISEntityManager"/>
		</xs:choice>
	</xs:group>
	<xs:group name="ChildContentModelGeoSpatial">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DChildNode for GeoSpatial component.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geodata.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="GeoLocation"/>
			<xs:element ref="GeoLOD"/>
			<xs:element ref="GeoMetadata"/>
			<xs:element ref="GeoOrigin"/>
			<xs:element ref="GeoPositionInterpolator"/>
			<xs:element ref="GeoProximitySensor"/>
			<xs:element ref="GeoTouchSensor"/>
			<xs:element ref="GeoViewpoint"/>
			<xs:element ref="GeoTransform"/>
		</xs:choice>
	</xs:group>
	<xs:group name="ChildContentModelHumanoidAnimation">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DChildNode for HumanoidAnimation component.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/hanim.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="HAnimHumanoid"/>
			<xs:element ref="HAnimJoint"/>
			<xs:element ref="HAnimSegment"/>
			<xs:element ref="HAnimSite"/>
		</xs:choice>
	</xs:group>
	<xs:group name="ChildContentModelNurbs">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DChildNode for Nurbs component.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="NurbsOrientationInterpolator"/>
			<xs:element ref="NurbsPositionInterpolator"/>
			<xs:element ref="NurbsSurfaceInterpolator"/>
			<xs:element ref="NurbsSet"/>
		</xs:choice>
	</xs:group>
	<xs:group name="ChildContentModelProtoInstance">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to ProtoInstance in Immersive profile.</xs:appinfo>
			<xs:documentation/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="ProtoInstance">
				<xs:annotation>
					<xs:documentation>Appropriately typed substitute node</xs:documentation>
				</xs:annotation>
			</xs:element>
		</xs:choice>
	</xs:group>
	<xs:group name="ChildContentModelSceneGraphStructure">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to SceneGraphStructure elements, which are not specific X3D nodes.</xs:appinfo>
			<xs:documentation/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="ROUTE"/>
			<xs:element ref="ExternProtoDeclare"/>
			<xs:element ref="ProtoDeclare"/>
			<xs:element ref="IMPORT"/>
			<xs:element ref="EXPORT"/>
		</xs:choice>
	</xs:group>
	<xs:group name="ColorCoordinateContentModel">
		<xs:annotation>
			<xs:appinfo>ColorCoordinateContentModel is the child-node content model corresponding to IndexedLineSet, LineSet and PointSet.  ColorCoordinateContentModel can contain any-order Coordinate node with Color (or ColorRGBA) node.  No more than one instance of any single node type is allowed.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#IndexedLineSet"/>
		</xs:annotation>
		<xs:choice>
			<xs:sequence-cl>
				<xs:choice>
					<xs:element ref="Color"/>
					<xs:element ref="ColorRGBA"/>
				</xs:choice>
				<xs:choice minOccurs="0">
					<xs:element ref="Coordinate"/>
					<xs:element ref="CoordinateDouble"/>
					<xs:element ref="GeoCoordinate"/>
					<xs:element ref="ProtoInstance">
						<xs:annotation>
							<xs:documentation>Appropriately typed substitute node</xs:documentation>
						</xs:annotation>
					</xs:element>
				</xs:choice>
			</xs:sequence-cl>
			<xs:sequence-cl>
				<xs:choice>
					<xs:element ref="Coordinate"/>
					<xs:element ref="CoordinateDouble"/>
					<xs:element ref="GeoCoordinate"/>
				</xs:choice>
				<xs:choice minOccurs="0">
					<xs:choice>
						<xs:element ref="Color"/>
						<xs:element ref="ColorRGBA"/>
					</xs:choice>
					<xs:element ref="ProtoInstance">
						<xs:annotation>
							<xs:documentation>Appropriately typed substitute node</xs:documentation>
						</xs:annotation>
					</xs:element>
				</xs:choice>
			</xs:sequence-cl>
			<xs:sequence-cl>
				<xs:element ref="ProtoInstance">
					<xs:annotation>
						<xs:documentation>Appropriately typed substitute node</xs:documentation>
					</xs:annotation>
				</xs:element>
				<xs:choice minOccurs="0">
					<xs:choice>
						<xs:element ref="Color"/>
						<xs:element ref="ColorRGBA"/>
					</xs:choice>
					<xs:choice>
						<xs:element ref="Coordinate"/>
						<xs:element ref="CoordinateDouble"/>
						<xs:element ref="GeoCoordinate"/>
					</xs:choice>
					<xs:element ref="ProtoInstance">
						<xs:annotation>
							<xs:documentation>Appropriately typed substitute node</xs:documentation>
						</xs:annotation>
					</xs:element>
				</xs:choice>
			</xs:sequence-cl>
		</xs:choice>
	</xs:group>
	<xs:group name="ColorNormalTexCoordContentModel">
		<xs:annotation>
			<xs:appinfo>ColorNormalTexCoordContentModel is the child-node content model corresponding to ElevationGrid and GeoElevationGrid.  ColorNormalTexCoordContentModel can contain Color (or ColorRGBA), Normal and TextureCoordinate, in any order.  No more than one instance of any single node type is allowed.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry3D.html#ElevationGrid"/>
		</xs:annotation>
		<xs:sequence-cl>
			<xs:choice>
				<xs:sequence-cl>
					<xs:choice>
						<xs:element ref="Color"/>
						<xs:element ref="ColorRGBA"/>
					</xs:choice>
					<xs:choice minOccurs="0">
						<xs:sequence-cl>
							<xs:element ref="Normal"/>
							<xs:choice minOccurs="0">
								<xs:element ref="TextureCoordinate"/>
								<xs:element ref="TextureCoordinateGenerator"/>
								<xs:element ref="MultiTextureCoordinate"/>
								<xs:element ref="NurbsTextureCoordinate"/>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:choice>
								<xs:element ref="TextureCoordinate"/>
								<xs:element ref="TextureCoordinateGenerator"/>
								<xs:element ref="MultiTextureCoordinate"/>
								<xs:element ref="NurbsTextureCoordinate"/>
							</xs:choice>
							<xs:choice minOccurs="0">
								<xs:element ref="Normal"/>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:choice minOccurs="0">
								<xs:element ref="Normal"/>
								<xs:element ref="TextureCoordinate"/>
								<xs:element ref="TextureCoordinateGenerator"/>
								<xs:element ref="MultiTextureCoordinate"/>
								<xs:element ref="NurbsTextureCoordinate"/>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
					</xs:choice>
				</xs:sequence-cl>
				<xs:sequence-cl>
					<xs:element ref="Normal"/>
					<xs:choice minOccurs="0">
						<xs:sequence-cl>
							<xs:choice>
								<xs:element ref="Color"/>
								<xs:element ref="ColorRGBA"/>
							</xs:choice>
							<xs:choice minOccurs="0">
								<xs:element ref="TextureCoordinate"/>
								<xs:element ref="TextureCoordinateGenerator"/>
								<xs:element ref="MultiTextureCoordinate"/>
								<xs:element ref="NurbsTextureCoordinate"/>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:choice>
								<xs:element ref="TextureCoordinate"/>
								<xs:element ref="TextureCoordinateGenerator"/>
								<xs:element ref="MultiTextureCoordinate"/>
								<xs:element ref="NurbsTextureCoordinate"/>
							</xs:choice>
							<xs:choice minOccurs="0">
								<xs:element ref="Color"/>
								<xs:element ref="ColorRGBA"/>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:choice minOccurs="0">
								<xs:element ref="Color"/>
								<xs:element ref="ColorRGBA"/>
								<xs:element ref="TextureCoordinate"/>
								<xs:element ref="TextureCoordinateGenerator"/>
								<xs:element ref="MultiTextureCoordinate"/>
								<xs:element ref="NurbsTextureCoordinate"/>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
					</xs:choice>
				</xs:sequence-cl>
				<xs:sequence-cl>
					<xs:choice>
						<xs:element ref="TextureCoordinate"/>
						<xs:element ref="TextureCoordinateGenerator"/>
						<xs:element ref="MultiTextureCoordinate"/>
						<xs:element ref="NurbsTextureCoordinate"/>
					</xs:choice>
					<xs:choice minOccurs="0">
						<xs:sequence-cl>
							<xs:choice>
								<xs:element ref="Color"/>
								<xs:element ref="ColorRGBA"/>
							</xs:choice>
							<xs:choice minOccurs="0">
								<xs:element ref="Normal"/>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:element ref="Normal"/>
							<xs:choice minOccurs="0">
								<xs:element ref="Color"/>
								<xs:element ref="ColorRGBA"/>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:choice minOccurs="0">
								<xs:element ref="Color"/>
								<xs:element ref="ColorRGBA"/>
								<xs:element ref="Normal"/>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
					</xs:choice>
				</xs:sequence-cl>
				<xs:sequence-cl>
					<xs:element ref="ProtoInstance">
						<xs:annotation>
							<xs:documentation>Appropriately typed substitute node</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:choice minOccurs="0">
						<xs:sequence-cl>
							<xs:choice>
								<xs:element ref="Color"/>
								<xs:element ref="ColorRGBA"/>
							</xs:choice>
							<xs:choice minOccurs="0">
								<xs:element ref="Normal"/>
								<xs:element ref="TextureCoordinate"/>
								<xs:element ref="TextureCoordinateGenerator"/>
								<xs:element ref="MultiTextureCoordinate"/>
								<xs:element ref="NurbsTextureCoordinate"/>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:element ref="Normal"/>
							<xs:choice minOccurs="0">
								<xs:element ref="Color"/>
								<xs:element ref="ColorRGBA"/>
								<xs:element ref="TextureCoordinate"/>
								<xs:element ref="TextureCoordinateGenerator"/>
								<xs:element ref="MultiTextureCoordinate"/>
								<xs:element ref="NurbsTextureCoordinate"/>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:choice minOccurs="0">
								<xs:element ref="Color"/>
								<xs:element ref="ColorRGBA"/>
								<xs:element ref="Normal"/>
								<xs:element ref="TextureCoordinate"/>
								<xs:element ref="TextureCoordinateGenerator"/>
								<xs:element ref="MultiTextureCoordinate"/>
								<xs:element ref="NurbsTextureCoordinate"/>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
					</xs:choice>
				</xs:sequence-cl>
			</xs:choice>
			<xs:element ref="FogCoordinate" minOccurs="0"/>
		</xs:sequence-cl>
	</xs:group>
	<xs:group name="ComposedGeometryContentModel">
		<xs:annotation>
			<xs:appinfo>ComposedGeometryContentModel is the child-node content model corresponding to X3DComposedGeometryNodes.  It can contain Color (or ColorRGBA), Coordinate, Normal and TextureCoordinate, in any order.  No more than one instance of these nodes is allowed.  Multiple VertexAttribute (FloatVertexAttribute, Matrix3VertexAttribute, Matrix4VertexAttribute) nodes can also be contained.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry3D.html#IndexedFaceSet"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="FloatVertexAttribute">
				<xs:annotation>
					<xs:documentation>attrib</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="Matrix3VertexAttribute">
				<xs:annotation>
					<xs:documentation>attrib</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="Matrix4VertexAttribute">
				<xs:annotation>
					<xs:documentation>attrib</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="Color">
				<xs:annotation>
					<xs:documentation>color</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="ColorRGBA">
				<xs:annotation>
					<xs:documentation>color</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="Coordinate">
				<xs:annotation>
					<xs:documentation>coord</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="CoordinateDouble">
				<xs:annotation>
					<xs:documentation>coord</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="GeoCoordinate">
				<xs:annotation>
					<xs:documentation>coord</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="FogCoordinate">
				<xs:annotation>
					<xs:documentation>fogcoord</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="Normal">
				<xs:annotation>
					<xs:documentation>normal</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="TextureCoordinate">
				<xs:annotation>
					<xs:documentation>texcoord</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="TextureCoordinate3D">
				<xs:annotation>
					<xs:documentation>texcoord</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="TextureCoordinate4D">
				<xs:annotation>
					<xs:documentation>texcoord</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="TextureCoordinateGenerator">
				<xs:annotation>
					<xs:documentation>texcoord</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="MultiTextureCoordinate">
				<xs:annotation>
					<xs:documentation>texcoord</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="NurbsTextureCoordinate">
				<xs:annotation>
					<xs:documentation>texcoord</xs:documentation>
				</xs:annotation>
			</xs:element>
			<xs:element ref="ProtoInstance">
				<xs:annotation>
					<xs:documentation>Appropriately typed substitute node</xs:documentation>
				</xs:annotation>
			</xs:element>
		</xs:choice>
	</xs:group>
	<xs:group name="ColorCoordinateNormalTexCoordContentModel">
		<xs:annotation>
			<xs:appinfo>ColorCoordinateNormalTexCoordContentModel is the child-node content model corresponding to IndexedFaceSet and related Triangle 3D geometry nodes.  ColorCoordinateNormalTexCoordContentModel can contain VertexAttribute, Color (or ColorRGBA), Coordinate (or CoordinateDouble), Normal and TextureCoordinate nodes, in any order.  No more than one instance of any single node type is allowed.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry3D.html#IndexedFaceSet"/>
		</xs:annotation>
		<xs:sequence-cl>
			<xs:choice>
				<xs:sequence-cl>
					<xs:choice>
						<xs:element ref="Color"/>
						<xs:element ref="ColorRGBA"/>
					</xs:choice>
					<xs:choice minOccurs="0">
						<xs:sequence-cl>
							<xs:choice>
								<xs:element ref="Coordinate"/>
								<xs:element ref="CoordinateDouble"/>
								<xs:element ref="GeoCoordinate"/>
							</xs:choice>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:element ref="Normal"/>
									<xs:choice minOccurs="0">
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Normal"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:element ref="Normal"/>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:choice>
								<xs:element ref="TextureCoordinate"/>
								<xs:element ref="TextureCoordinateGenerator"/>
								<xs:element ref="MultiTextureCoordinate"/>
								<xs:element ref="NurbsTextureCoordinate"/>
							</xs:choice>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="Normal"/>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Normal"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="Normal"/>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="Normal"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
					</xs:choice>
				</xs:sequence-cl>
				<xs:sequence-cl>
					<xs:choice>
						<xs:element ref="Coordinate"/>
						<xs:element ref="CoordinateDouble"/>
						<xs:element ref="GeoCoordinate"/>
					</xs:choice>
					<xs:choice minOccurs="0">
						<xs:sequence-cl>
							<xs:choice>
								<xs:element ref="Color"/>
								<xs:element ref="ColorRGBA"/>
							</xs:choice>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:element ref="Normal"/>
									<xs:choice minOccurs="0">
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Normal"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:element ref="Normal"/>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:choice>
								<xs:element ref="TextureCoordinate"/>
								<xs:element ref="TextureCoordinateGenerator"/>
								<xs:element ref="MultiTextureCoordinate"/>
								<xs:element ref="NurbsTextureCoordinate"/>
							</xs:choice>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="Normal"/>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Normal"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="Normal"/>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Normal"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
					</xs:choice>
				</xs:sequence-cl>
				<xs:sequence-cl>
					<xs:element ref="Normal"/>
					<xs:choice minOccurs="0">
						<xs:sequence-cl>
							<xs:choice>
								<xs:element ref="Color"/>
								<xs:element ref="ColorRGBA"/>
							</xs:choice>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:choice>
								<xs:element ref="Coordinate"/>
								<xs:element ref="CoordinateDouble"/>
								<xs:element ref="GeoCoordinate"/>
							</xs:choice>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:choice>
								<xs:element ref="TextureCoordinate"/>
								<xs:element ref="TextureCoordinateGenerator"/>
								<xs:element ref="MultiTextureCoordinate"/>
								<xs:element ref="NurbsTextureCoordinate"/>
							</xs:choice>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
					</xs:choice>
				</xs:sequence-cl>
				<xs:sequence-cl>
					<xs:choice>
						<xs:element ref="TextureCoordinate"/>
						<xs:element ref="TextureCoordinateGenerator"/>
						<xs:element ref="MultiTextureCoordinate"/>
						<xs:element ref="NurbsTextureCoordinate"/>
					</xs:choice>
					<xs:choice minOccurs="0">
						<xs:sequence-cl>
							<xs:choice>
								<xs:element ref="Color"/>
								<xs:element ref="ColorRGBA"/>
							</xs:choice>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="Normal"/>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:choice>
								<xs:element ref="Coordinate"/>
								<xs:element ref="CoordinateDouble"/>
								<xs:element ref="GeoCoordinate"/>
							</xs:choice>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="Normal"/>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:element ref="Normal"/>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="Normal"/>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
					</xs:choice>
				</xs:sequence-cl>
				<xs:sequence-cl>
					<xs:element ref="ProtoInstance">
						<xs:annotation>
							<xs:documentation>Appropriately typed substitute node</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:choice minOccurs="0">
						<xs:sequence-cl>
							<xs:choice>
								<xs:element ref="Color"/>
								<xs:element ref="ColorRGBA"/>
							</xs:choice>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Normal"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="Normal"/>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="Normal"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:choice>
								<xs:element ref="Coordinate"/>
								<xs:element ref="CoordinateDouble"/>
								<xs:element ref="GeoCoordinate"/>
							</xs:choice>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Normal"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="Normal"/>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Normal"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:element ref="Normal"/>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:choice>
								<xs:element ref="TextureCoordinate"/>
								<xs:element ref="TextureCoordinateGenerator"/>
								<xs:element ref="MultiTextureCoordinate"/>
								<xs:element ref="NurbsTextureCoordinate"/>
							</xs:choice>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="Normal"/>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:choice minOccurs="0">
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="Normal"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Normal"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="Normal"/>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:choice>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
									</xs:choice>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="Normal"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
								<xs:sequence-cl>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
									<xs:choice minOccurs="0">
										<xs:element ref="Color"/>
										<xs:element ref="ColorRGBA"/>
										<xs:element ref="Coordinate"/>
										<xs:element ref="CoordinateDouble"/>
										<xs:element ref="GeoCoordinate"/>
										<xs:element ref="Normal"/>
										<xs:element ref="TextureCoordinate"/>
										<xs:element ref="TextureCoordinateGenerator"/>
										<xs:element ref="MultiTextureCoordinate"/>
										<xs:element ref="NurbsTextureCoordinate"/>
										<xs:element ref="ProtoInstance">
											<xs:annotation>
												<xs:documentation>Appropriately typed substitute node</xs:documentation>
											</xs:annotation>
										</xs:element>
									</xs:choice>
								</xs:sequence-cl>
							</xs:choice>
						</xs:sequence-cl>
					</xs:choice>
				</xs:sequence-cl>
			</xs:choice>
			<xs:element ref="FogCoordinate" minOccurs="0"/>
		</xs:sequence-cl>
	</xs:group>
	<xs:group name="GeometryContentModel">
		<xs:annotation>
			<xs:appinfo>GeometryContentModel is the child-node content model corresponding to X3DGeometryNode.   No more than one instance of any single geometry node is allowed.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry3D.html#Shapeandgeometry">geometry</xs:documentation>
		</xs:annotation>
		<xs:choice>
			<xs:group ref="GeometryContentModelInterchange"/>
			<xs:group ref="GeometryContentModelInteractive"/>
			<xs:group ref="GeometryContentModelImmersive"/>
			<xs:group ref="GeometryContentModel2D"/>
			<xs:group ref="GeometryContentModelCAD"/>
			<xs:group ref="GeometryContentModelGeoSpatial"/>
			<xs:group ref="GeometryContentModelNurbs"/>
		</xs:choice>
	</xs:group>
	<xs:group name="GeometryContentModelInterchange">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DGeometryNode for Interchange profile.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/interchange.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="Box"/>
			<xs:element ref="Cone"/>
			<xs:element ref="Cylinder"/>
			<xs:element ref="IndexedFaceSet"/>
			<xs:element ref="IndexedLineSet"/>
			<xs:element ref="IndexedTriangleFanSet"/>
			<xs:element ref="IndexedTriangleSet"/>
			<xs:element ref="IndexedTriangleStripSet"/>
			<xs:element ref="LineSet"/>
			<xs:element ref="PointSet"/>
			<xs:element ref="Sphere"/>
			<xs:element ref="TriangleFanSet"/>
			<xs:element ref="TriangleSet"/>
			<xs:element ref="TriangleStripSet"/>
		</xs:choice>
	</xs:group>
	<xs:group name="GeometryContentModelInteractive">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DGeometryNode for Interactive profile.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/interactive.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="ElevationGrid"/>
		</xs:choice>
	</xs:group>
	<xs:group name="GeometryContentModelImmersive">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DGeometryNode for Immersive profile.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/immersive.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:group ref="GeometryContentModel2DImmersive"/>
			<xs:element ref="Extrusion"/>
			<xs:element ref="Text"/>
		</xs:choice>
	</xs:group>
	<xs:group name="GeometryContentModel2D">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to Geometry2D component.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry2D.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="Arc2D"/>
			<xs:element ref="ArcClose2D"/>
			<xs:element ref="Circle2D"/>
			<xs:element ref="Disk2D"/>
		</xs:choice>
	</xs:group>
	<xs:group name="GeometryContentModelCAD">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to nodes in CAD component.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry2D.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="QuadSet"/>
			<xs:element ref="IndexedQuadSet"/>
		</xs:choice>
	</xs:group>
	<xs:group name="GeometryContentModel2DImmersive">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to Geometry2D component.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry2D.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="Polyline2D"/>
			<xs:element ref="Polypoint2D"/>
			<xs:element ref="Rectangle2D"/>
			<xs:element ref="TriangleSet2D"/>
		</xs:choice>
	</xs:group>
	<xs:group name="GeometryContentModelGeoSpatial">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DGeometryNode for GeoSpatial component.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geodata.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="GeoElevationGrid"/>
		</xs:choice>
	</xs:group>
	<xs:group name="GeometryContentModelNurbs">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DGeometryNode for Nurbs component.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="NurbsCurve"/>
			<xs:element ref="NurbsCurve2D"/>
			<xs:element ref="NurbsPatchSurface"/>
			<xs:element ref="NurbsSweptSurface"/>
			<xs:element ref="NurbsSwungSurface"/>
			<xs:element ref="NurbsTrimmedSurface"/>
		</xs:choice>
	</xs:group>
	<xs:group name="GroupingNodeChildContentModel">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DChildNode.  GroupingNodeChildContentModel can contain most nodes, other Grouping nodes, Prototype declarations and ProtoInstances in any order and any combination.  When less that Full profile, the precise palette of legal nodes that are available depends on profile and components.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#f-Objecthierarchy"/>
		</xs:annotation>
		<xs:choice>
			<xs:sequence-cl minOccurs="0" maxOccurs="unbounded">
				<xs:group ref="ChildContentModel" minOccurs="0" maxOccurs="unbounded"/>
				<xs:group ref="ChildContentModelSceneGraphStructure" minOccurs="0" maxOccurs="unbounded"/>
			</xs:sequence-cl>
		</xs:choice>
	</xs:group>
	<xs:group name="LoadSensorChildContentModel">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to LoadSensor node, which determines if the retrievable content for the child X3DUrlObject nodes has been loaded from the network.  When less that Full profile, the precise palette of legal nodes that are available depends on profile and components.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#f-Objecthierarchy"/>
		</xs:annotation>
                <xs:choice>
                        <xs:annotation>
                                <xs:documentation>watchList</xs:documentation>
                        </xs:annotation>
                        <xs:element ref="AudioClip"/>
                        <xs:element ref="ImageCubeMapTexture"/>
                        <xs:element ref="ImageTexture"/>
                        <xs:element ref="ImageTexture3D"/>
                        <xs:element ref="Inline"/>
                        <xs:element ref="MovieTexture"/>
                        <xs:element ref="PackagedShader"/>
                        <xs:element ref="ShaderPart"/>
                        <xs:element ref="ShaderProgram"/>
                </xs:choice>
	</xs:group>
	<xs:group name="SceneGraphFragmentContentModel">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to a valid scene-graph fragment.  SceneGraphFragmentContentModel can provide field or fieldValue initialization, so no ROUTEs or prototype declarations allowed.</xs:appinfo>
			<xs:documentation/>
		</xs:annotation>
		<xs:choice>
			<xs:group ref="AppearanceChildContentModelNoProtoInstance"/>
			<xs:group ref="ChildContentModelCore"/>
			<xs:group ref="ChildContentModel"/>
			<xs:group ref="GeometryContentModel"/>
			<xs:element ref="Color"/>
			<xs:element ref="ColorRGBA"/>
			<xs:element ref="Coordinate"/>
			<xs:element ref="CoordinateDouble"/>
			<xs:element ref="FontStyle"/>
			<xs:element ref="ScreenFontStyle"/>
			<xs:element ref="GeoCoordinate"/>
			<xs:element ref="Normal"/>
			<xs:element ref="TextureCoordinate"/>
			<xs:element ref="HAnimDisplacer"/>
			<xs:element ref="Contour2D"/>
			<xs:element ref="ContourPolyline2D"/>
			<xs:element ref="NurbsTextureCoordinate"/>
			<xs:element ref="Layer"/>
			<xs:element ref="LayoutLayer"/>
			<xs:element ref="Viewport"/>
			<xs:element ref="BallJoint"/>
			<xs:element ref="CollidableOffset"/>
			<xs:element ref="CollisionCollection"/>
			<xs:element ref="CollisionSpace"/>
			<xs:element ref="Contact"/>
			<xs:element ref="DoubleAxisHingeJoint"/>
			<xs:element ref="MotorJoint"/>
			<xs:element ref="RigidBody"/>
			<xs:element ref="SingleAxisHingeJoint"/>
			<xs:element ref="SliderJoint"/>
			<xs:element ref="UniversalJoint"/>
		</xs:choice>
	</xs:group>
	<xs:group name="SceneGraphFragmentWithPrototypeDeclarationsContentModel">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to a valid scene-graph fragment.  Also includes ROUTE(s) and prototype declaration(s).</xs:appinfo>
			<xs:documentation/>
		</xs:annotation>
		<xs:choice>
			<xs:choice minOccurs="0" maxOccurs="unbounded">
				<xs:group ref="SceneGraphFragmentContentModel" minOccurs="0" maxOccurs="unbounded"/>
				<xs:group ref="ChildContentModelSceneGraphStructure" minOccurs="0" maxOccurs="unbounded"/>
			</xs:choice>
		</xs:choice>
	</xs:group>
	<xs:group name="ShapeChildContentModel">
		<xs:annotation>
			<xs:appinfo>ShapeChildContentModel is the child-node content model corresponding to X3DShapeNode.  ShapeChildContentModel can contain a single Appearance node and a single geometry node, in any order.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shape.html#Shapenodes"/>
		</xs:annotation>
		<xs:choice>
			<xs:sequence-cl>
				<xs:element ref="Appearance">
					<xs:annotation>
						<xs:documentation>appearance</xs:documentation>
					</xs:annotation>
				</xs:element>
				<xs:choice minOccurs="0">
					<xs:annotation>
						<xs:documentation>geometry</xs:documentation>
					</xs:annotation>
					<xs:group ref="GeometryContentModel"/>
					<xs:element ref="ProtoInstance">
						<xs:annotation>
							<xs:documentation>Appropriately typed substitute node</xs:documentation>
						</xs:annotation>
					</xs:element>
				</xs:choice>
			</xs:sequence-cl>
			<xs:sequence-cl>
				<xs:group ref="GeometryContentModel">
					<xs:annotation>
						<xs:documentation>geometry</xs:documentation>
					</xs:annotation>
				</xs:group>
				<xs:choice minOccurs="0">
					<xs:annotation>
						<xs:documentation>appearance</xs:documentation>
					</xs:annotation>
					<xs:element ref="Appearance"/>
					<xs:element ref="ProtoInstance">
						<xs:annotation>
							<xs:documentation>Appropriately typed substitute node</xs:documentation>
						</xs:annotation>
					</xs:element>
				</xs:choice>
			</xs:sequence-cl>
			<xs:sequence-cl>
				<xs:element ref="ProtoInstance">
					<xs:annotation>
						<xs:documentation>appearance or geometry</xs:documentation>
					</xs:annotation>
				</xs:element>
				<xs:choice minOccurs="0">
					<xs:annotation>
						<xs:documentation>geometry or appearance</xs:documentation>
					</xs:annotation>
					<xs:group ref="GeometryContentModel">
						<xs:annotation>
							<xs:documentation>geometry</xs:documentation>
						</xs:annotation>
					</xs:group>
					<xs:element ref="Appearance">
						<xs:annotation>
							<xs:documentation>appearance</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="ProtoInstance">
						<xs:annotation>
							<xs:documentation>Appropriately typed substitute node</xs:documentation>
						</xs:annotation>
					</xs:element>
				</xs:choice>
			</xs:sequence-cl>
		</xs:choice>
	</xs:group>
	<xs:group name="SoundChildContentModel">
		<xs:annotation>
			<xs:appinfo>SoundChildContentModel is the child-node content model corresponding to X3DSoundNode.  SoundChildContentModel can contain a single AudioClip or MovieTexture as sound source.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/sound.html#Sound"/>
		</xs:annotation>
		<xs:choice>
			<xs:annotation>
				<xs:documentation>source</xs:documentation>
			</xs:annotation>
			<xs:element ref="AudioClip"/>
			<xs:element ref="MovieTexture"/>
			<xs:element ref="ProtoInstance">
				<xs:annotation>
					<xs:documentation>Appropriately typed substitute node</xs:documentation>
				</xs:annotation>
			</xs:element>
		</xs:choice>
	</xs:group>
	<xs:group name="TextChildContentModel">
		<xs:annotation>
			<xs:appinfo>TextChildContentModel is the child-node content model corresponding to X3DTextNode.  TextChildContentModel can contain a single FontStyle node.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/text.html#Text"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="FontStyle"/>
			<xs:element ref="ScreenFontStyle"/>
			<xs:element ref="ProtoInstance">
				<xs:annotation>
					<xs:documentation>Appropriately typed substitute node</xs:documentation>
				</xs:annotation>
			</xs:element>
		</xs:choice>
	</xs:group>
	<xs:group name="TextureBackgroundChildContentModel">
		<xs:annotation>
			<xs:appinfo>TextureBackgroundChildContentModel is the child-node content model corresponding to TextureBackground.  TextureBackgroundChildContentModel can contain up to six Texture nodes (ImageTexture, MovieTexture, MultiTexture or PixelTexture).  Each child texture must have a different containerField for topTexture, bottomTexture, leftTexture, rightTexture, frontTexture and backTexture.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/enveffects.html#TextureBackground"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="ImageTexture"/>
			<xs:element ref="MovieTexture"/>
			<xs:element ref="MultiTexture"/>
			<xs:element ref="PixelTexture"/>
			<xs:element ref="ProtoInstance">
				<xs:annotation>
					<xs:documentation>Appropriately typed substitute node</xs:documentation>
				</xs:annotation>
			</xs:element>
		</xs:choice>
	</xs:group>
	<xs:element name="Anchor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/networking.html#Anchor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode">
					<xs:attribute name="description" type="SFString"/>
					<xs:attribute name="parameter" type="MFString"/>
					<xs:attribute name="url" type="MFString"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Appearance">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shape.html#Appearance"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DAppearanceNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Arc2D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry2D.html#Arc2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:attribute name="radius" default="1">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minExclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="startAngle" default="0">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minExclusive value="-6.2832"/>
								<xs:maxExclusive value="6.2832"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="endAngle" default="1.570796">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minExclusive value="-6.2832"/>
								<xs:maxExclusive value="6.2832"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ArcClose2D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry2D.html#ArcClose2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:attribute name="radius" default="1">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minExclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="startAngle" default="0">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minExclusive value="-6.2832"/>
								<xs:maxExclusive value="6.2832"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="endAngle" default="1.570796">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minExclusive value="-6.2832"/>
								<xs:maxExclusive value="6.2832"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="closureType" type="ArcClose2dTypeValues" default="PIE"/>
					<xs:attribute name="solid" type="SFBool" default="false"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="AudioClip">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DUrlObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/sound.html#AudioClip"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DSoundSourceNode">
					<xs:attribute name="description" type="SFString"/>
					<xs:attribute name="url" type="MFString"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Background">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/enveffects.html#Background"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DBackgroundNode">
					<xs:attribute name="backUrl" type="MFString"/>
					<xs:attribute name="bottomUrl" type="MFString"/>
					<xs:attribute name="frontUrl" type="MFString"/>
					<xs:attribute name="leftUrl" type="MFString"/>
					<xs:attribute name="rightUrl" type="MFString"/>
					<xs:attribute name="topUrl" type="MFString"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Billboard">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/navigation.html#Billboard"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode">
					<xs:attribute name="axisOfRotation" type="SFVec3f" default="0 1 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="BooleanFilter">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/utils.html#BooleanFilter"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChildNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="BooleanSequencer">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/utils.html#BooleanSequencer"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DSequencerNode">
					<xs:attribute name="keyValue" type="MFBool"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="BooleanToggle">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/utils.html#BooleanToggle"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChildNode">
					<xs:attribute name="toggle" type="SFBool" default="false"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="BooleanTrigger">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/utils.html#BooleanTrigger"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChildNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Box">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry3D.html#Box"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:attribute name="size" type="SFVec3f" default="2 2 2"/>
					<xs:attribute name="solid" type="SFBool" default="true"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Circle2D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry2D.html#Circle2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:attribute name="radius" default="1">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minExclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ClipPlane">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#ClipPlane"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChildNode">
					<xs:attribute name="enabled" type="SFBool" default="true"/>
					<xs:attribute name="plane" type="SFVec4f" default="0 1 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Collision">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DSensorNode"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/navigation.html#Collision"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode">
					<xs:attribute name="enabled" type="SFBool" default="true"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Color">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#Color"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DColorNode">
					<xs:attribute name="color" type="MFColor"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ColorRGBA">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#ColorRGBA"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DColorNode">
					<xs:attribute name="color" type="MFColorRGBA"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ColorInterpolator">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/interp.html#ColorInterpolator"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DInterpolatorNode">
					<xs:attribute name="keyValue" type="MFColor"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Cone">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry3D.html#Cone"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:attribute name="bottomRadius" default="1">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minExclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="height" default="2">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minExclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="side" type="SFBool" default="true"/>
					<xs:attribute name="bottom" type="SFBool" default="true"/>
					<xs:attribute name="solid" type="SFBool" default="true"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Coordinate">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#Coordinate"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DCoordinateNode">
					<xs:attribute name="point" type="MFVec3f"/>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="coord"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="CoordinateDouble">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html#CoordinateDouble"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DCoordinateNode">
					<xs:attribute name="point" type="MFVec3d"/>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="coord"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="CoordinateInterpolator">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html#CoordinateDouble"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DInterpolatorNode">
					<xs:attribute name="keyValue" type="MFVec3f"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="CoordinateInterpolator2D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/interp.html#CoordinateInterpolator2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DInterpolatorNode">
					<xs:attribute name="keyValue" type="MFVec2f"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Cylinder">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry3D.html#Cylinder"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:attribute name="bottom" type="SFBool" default="true"/>
					<xs:attribute name="height" default="2">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minExclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="radius" default="1">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minExclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="side" type="SFBool" default="true"/>
					<xs:attribute name="top" type="SFBool" default="true"/>
					<xs:attribute name="solid" type="SFBool" default="true"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="CylinderSensor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/pointingsensor.html#CylinderSensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DDragSensorNode">
					<xs:attribute name="diskAngle" type="SFFloat" default="0.26179167"/>
					<xs:attribute name="maxAngle" type="SFFloat" default="-1"/>
					<xs:attribute name="minAngle" type="SFFloat" default="0"/>
					<xs:attribute name="offset" type="SFFloat" default="0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="DirectionalLight">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/lighting.html#DirectionalLight"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DLightNode">
					<xs:attribute name="direction" type="SFVec3f" default="0 0 -1"/>
					<xs:attribute name="global" type="SFBool" default="false"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Disk2D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry2D.html#Disk2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:attribute name="innerRadius" default="0">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="outerRadius" default="1">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minExclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="solid" type="SFBool" default="false"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ElevationGrid">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry3D.html#ElevationGrid"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:choice minOccurs="0" maxOccurs="unbounded">
						<xs:element ref="FloatVertexAttribute">
							<xs:annotation>
								<xs:documentation>attrib</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="Matrix3VertexAttribute">
							<xs:annotation>
								<xs:documentation>attrib</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="Matrix4VertexAttribute">
							<xs:annotation>
								<xs:documentation>attrib</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="Color">
							<xs:annotation>
								<xs:documentation>color</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="ColorRGBA">
							<xs:annotation>
								<xs:documentation>color</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="FogCoordinate">
							<xs:annotation>
								<xs:documentation>fogcoord</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="Normal">
							<xs:annotation>
								<xs:documentation>normal</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="TextureCoordinate">
							<xs:annotation>
								<xs:documentation>texcoord</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="TextureCoordinate3D">
							<xs:annotation>
								<xs:documentation>texcoord</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="TextureCoordinate4D">
							<xs:annotation>
								<xs:documentation>texcoord</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="TextureCoordinateGenerator">
							<xs:annotation>
								<xs:documentation>texcoord</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="MultiTextureCoordinate">
							<xs:annotation>
								<xs:documentation>texcoord</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="NurbsTextureCoordinate">
							<xs:annotation>
								<xs:documentation>texcoord</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="height" type="MFFloat"/>
					<xs:attribute name="ccw" type="SFBool" default="true"/>
					<xs:attribute name="colorPerVertex" type="SFBool" default="true"/>
					<xs:attribute name="creaseAngle" default="0">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="normalPerVertex" type="SFBool" default="true"/>
					<xs:attribute name="solid" type="SFBool" default="true"/>
					<xs:attribute name="xDimension" type="SFInt32" default="0"/>
					<xs:attribute name="xSpacing" default="1.0">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minExclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="zDimension" type="SFInt32" default="0"/>
					<xs:attribute name="zSpacing" default="1.0">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minExclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Extrusion">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry3D.html#Extrusion"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:attribute name="beginCap" type="SFBool" default="true"/>
					<xs:attribute name="ccw" type="SFBool" default="true"/>
					<xs:attribute name="convex" type="SFBool" default="true"/>
					<xs:attribute name="creaseAngle" default="0.0">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="crossSection" type="MFVec2f" default="1 1 1 -1 -1 -1 -1 1 1 1"/>
					<xs:attribute name="endCap" type="SFBool" default="true"/>
					<xs:attribute name="orientation" type="MFRotation" default="0 0 1 0"/>
					<xs:attribute name="scale" type="MFVec2f" default="1 1"/>
					<xs:attribute name="solid" type="SFBool" default="true"/>
					<xs:attribute name="spine" type="MFVec3f" default="0 0 0 0 1 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="FillProperties">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shape.html#FillProperties"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DAppearanceChildNode">
					<xs:attribute name="filled" type="SFBool" default="true"/>
					<xs:attribute name="hatched" type="SFBool" default="true"/>
					<xs:attribute name="hatchStyle" type="SFInt32" default="1"/>
					<xs:attribute name="hatchColor" type="SFColor" default="1 1 1"/>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="fillProperties"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Fog">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DFogObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/enveffects.html#Fog"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DBindableNode">
					<xs:attribute name="color" type="SFColor" default="1 1 1"/>
					<xs:attribute name="fogType" type="fogTypeValues" default="LINEAR"/>
					<xs:attribute name="visibilityRange" type="SFFloat" default="0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="FogCoordinate">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#Coordinate"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DCoordinateNode">
					<xs:attribute name="depth" type="MFVec3f"/>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="fogCoord"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="FontStyle">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/text.html#FontStyle"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DFontStyleNode">
					<xs:attribute name="family" type="MFString" default='"SERIF"'/>
					<xs:attribute name="horizontal" type="SFBool" default="true"/>
					<xs:attribute name="justify" type="MFString" default='"BEGIN"'/>
					<xs:attribute name="language" type="SFString"/>
					<xs:attribute name="leftToRight" type="SFBool" default="true"/>
					<xs:attribute name="size" type="SFFloat" default="1.0"/>
					<xs:attribute name="spacing" type="SFFloat" default="1.0"/>
					<xs:attribute name="style" type="fontStyleValues" default="PLAIN"/>
					<xs:attribute name="topToBottom" type="SFBool" default="true"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Group">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/group.html#Group"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ImageTexture">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DUrlObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#ImageTexture"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTexture2DNode">
					<xs:attribute name="url" type="MFString"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="IndexedFaceSet">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry3D.html#IndexedFaceSet"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposedGeometryNode">
					<xs:attribute name="convex" type="SFBool" default="true"/>
					<xs:attribute name="creaseAngle" default="0">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="colorIndex" type="MFInt32"/>
					<xs:attribute name="coordIndex" type="MFInt32"/>
					<xs:attribute name="normalIndex" type="MFInt32"/>
					<xs:attribute name="texCoordIndex" type="MFInt32"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="IndexedLineSet">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#IndexedLineSet"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:group ref="ColorCoordinateContentModel" minOccurs="0"/>
					<xs:attribute name="colorPerVertex" type="SFBool" default="true"/>
					<xs:attribute name="colorIndex" type="MFInt32"/>
					<xs:attribute name="coordIndex" type="MFInt32"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="IndexedTriangleFanSet">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#IndexedTriangleFanSet"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposedGeometryNode">
					<xs:attribute name="index" type="MFInt32"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="IndexedTriangleSet">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#IndexedTriangleSet"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposedGeometryNode">
					<xs:attribute name="index" type="MFInt32"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="IndexedTriangleStripSet">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#IndexedTriangleStripSet"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposedGeometryNode">
					<xs:attribute name="index" type="MFInt32"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Inline">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DUrlObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/networking.html#Inline"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChildNode">
					<xs:attribute name="load" type="SFBool" default="true"/>
					<xs:attribute name="bboxCenter" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="bboxSize" type="boundingBoxSizeType" default="-1 -1 -1"/>
					<xs:attribute name="url" type="MFString"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="IntegerSequencer">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/utils.html#IntegerSequencer"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DSequencerNode">
					<xs:attribute name="keyValue" type="MFInt32"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="IntegerTrigger">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/utils.html#IntegerTrigger"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChildNode">
					<xs:attribute name="integerKey" type="SFInt32" default="-1"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="KeySensor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/keyboard.html#KeySensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DKeyDeviceSensorNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="LineProperties">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shape.html#LineProperties"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DAppearanceChildNode">
					<xs:attribute name="applied" type="SFBool" default="true"/>
					<xs:attribute name="linetype" type="SFInt32" default="1"/>
					<xs:attribute name="linewidthScaleFactor" type="SFFloat" default="0"/>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="lineProperties"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TextureProperties">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shape.html#LineProperties"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DAppearanceChildNode">
					<xs:attribute name="anisotropicDegree" type="SFFloat" default="0"/>
					<xs:attribute name="borderColor" type="SFColorRGBA" default="0 0 0 0"/>
					<xs:attribute name="borderWidth" type="SFInt32" default="0"/>
					<xs:attribute name="boundaryModeS" type="textureBoundaryModeValues" default="REPEAT"/>
					<xs:attribute name="boundaryModeT" type="textureBoundaryModeValues" default="REPEAT"/>
					<xs:attribute name="boundaryModeR" type="textureBoundaryModeValues" default="REPEAT"/>
					<xs:attribute name="magnificationFilter" type="textureMagnificationModeValues" default="FASTEST"/>
					<xs:attribute name="minificationFilter" type="textureMinificationModeValues" default="FASTEST"/>
					<xs:attribute name="textureCompression" type="textureCompressionModeValues" default="FASTEST"/>
					<xs:attribute name="texturePriority" type="SFFloat" default="0"/>
					<xs:attribute name="generateMipMaps" type="SFBool" default="false"/>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="textureProperties"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="LineSet">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#LineSet"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:group ref="ColorCoordinateContentModel" minOccurs="0"/>
					<xs:attribute name="vertexCount" type="MFInt32"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="LoadSensor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/networking.html#LoadSensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNetworkSensorNode">
                                        <xs:group ref="LoadSensorChildContentModel" minOccurs="0" maxOccurs="unbounded"/>
                                        <xs:attribute name="timeOut" type="SFTime" default="0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="LocalFog">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DFogObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/enveffects.html#LocalFog"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DBindableNode">
					<xs:attribute name="enabled" type="SFBool" default="true"/>
					<xs:attribute name="color" type="SFColor" default="1 1 1"/>
					<xs:attribute name="fogType" type="fogTypeValues" default="LINEAR"/>
					<xs:attribute name="visibilityRange" type="SFFloat" default="0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="LOD">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/navigation.html#LOD"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode">
					<xs:attribute name="forceTransitions" type="SFBool" default="false"/>
					<xs:attribute name="center" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="range" type="MFFloat"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Material">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shape.html#Material"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DMaterialNode">
					<xs:attribute name="ambientIntensity" type="intensityType" default="0.2"/>
					<xs:attribute name="diffuseColor" type="SFColor" default="0.8 0.8 0.8"/>
					<xs:attribute name="emissiveColor" type="SFColor" default="0 0 0"/>
					<xs:attribute name="shininess" type="intensityType" default="0.2"/>
					<xs:attribute name="specularColor" type="SFColor" default="0 0 0"/>
					<xs:attribute name="transparency" type="intensityType" default="0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TwoSidedMaterial">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shape.html#Material"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DMaterialNode">
					<xs:attribute name="ambientIntensity" type="intensityType" default="0.2"/>
					<xs:attribute name="backAmbientIntensity" type="intensityType" default="0.2"/>
					<xs:attribute name="diffuseColor" type="SFColor" default="0.8 0.8 0.8"/>
					<xs:attribute name="backDiffuseColor" type="SFColor" default="0.8 0.8 0.8"/>
					<xs:attribute name="emissiveColor" type="SFColor" default="0 0 0"/>
					<xs:attribute name="backEmissiveColor" type="SFColor" default="0 0 0"/>
					<xs:attribute name="shininess" type="intensityType" default="0.2"/>
					<xs:attribute name="backShininess" type="intensityType" default="0.2"/>
					<xs:attribute name="specularColor" type="SFColor" default="0 0 0"/>
					<xs:attribute name="backSpecularColor" type="SFColor" default="0 0 0"/>
					<xs:attribute name="transparency" type="intensityType" default="0"/>
					<xs:attribute name="backTransparency" type="intensityType" default="0"/>
					<xs:attribute name="separateBackColor" type="SFBool" default="false"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="MetadataBoolean">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/core.html#MetadataBoolean"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DMetadataObject">
					<xs:attribute name="value" type="MFBool"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="MetadataDouble">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/core.html#MetadataDouble"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DMetadataObject">
					<xs:attribute name="value" type="MFDouble"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="MetadataFloat">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/core.html#MetadataFloat"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DMetadataObject">
					<xs:attribute name="value" type="MFFloat"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="MetadataInteger">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/core.html#MetadataInteger"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DMetadataObject">
					<xs:attribute name="value" type="MFInt32"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="MetadataSet">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DMetadataObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/core.html#MetadataSet"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="SceneGraphStructureNodeType">
					<xs:sequence-cl>
						<xs:element ref="IS" minOccurs="0"/>
						<xs:group ref="ChildContentModelCore" minOccurs="0" maxOccurs="unbounded"/>
					</xs:sequence-cl>
					<xs:attributeGroup ref="DEF_USE"/>
					<xs:attributeGroup ref="globalAttributes"/>
					<xs:attribute name="name" type="SFString"/>
					<xs:attribute name="reference" type="SFString"/>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="metadata">
						<xs:annotation>
							<xs:appinfo>containerField='value' for contained payload metadata inside MetadataSet element.</xs:appinfo>
						</xs:annotation>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="MetadataString">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/core.html#MetadataString"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DMetadataObject">
					<xs:attribute name="value" type="MFString"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="MovieTexture">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DTexture2DNode"/>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DUrlObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#MovieTexture"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DSoundSourceNode">
					<xs:attribute name="description" type="SFString"/>
					<xs:attribute name="url" type="MFString"/>
					<xs:attribute name="repeatS" type="SFBool" default="true"/>
					<xs:attribute name="repeatT" type="SFBool" default="true"/>
					<xs:attribute name="speed" type="SFFloat" default="1.0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="MultiTexture">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#MultiTexture"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTextureNode">
					<xs:choice minOccurs="0" maxOccurs="unbounded">
						<xs:element ref="ImageTexture"/>
						<xs:element ref="MovieTexture"/>
						<xs:element ref="PixelTexture"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="alpha" type="SFFloat" default="1"/>
					<xs:attribute name="color" type="SFColor" default="1 1 1"/>
					<xs:attribute name="function" type="MFString"/>
					<xs:attribute name="mode" type="MFString"/>
					<xs:attribute name="source" type="MFString"/>
					<xs:attribute name="transparent" type="SFBool" default="false"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="MultiTextureCoordinate">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#MultiTextureCoordinate"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTextureCoordinateNode">
					<xs:choice minOccurs="0" maxOccurs="unbounded">
						<xs:element ref="TextureCoordinate"/>
						<xs:element ref="TextureCoordinateGenerator"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="MultiTextureTransform">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#MultiTextureTransform"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTextureTransformNode">
					<xs:choice minOccurs="0" maxOccurs="unbounded">
						<xs:element ref="TextureTransform"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="NavigationInfo">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/navigation.html#NavigationInfo"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DBindableNode">
					<xs:attribute name="avatarSize" type="MFFloat" default="0.25 1.6 0.75"/>
					<xs:attribute name="headlight" type="SFBool" default="true"/>
					<xs:attribute name="speed" type="SFFloat" default="1"/>
					<xs:attribute name="type" type="MFString" default='"EXAMINE" "ANY"'/>
					<xs:attribute name="transitionType" type="MFString" default='"ANIMATE"'/>
					<xs:attribute name="transitionTime" type="MFFloat" default="1.0"/>
					<xs:attribute name="visibilityLimit" type="SFFloat" default="0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Normal">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#Normal"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNormalNode">
					<xs:attribute name="vector" type="MFVec3f"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="NormalInterpolator">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/interp.html#NormalInterpolator"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DInterpolatorNode">
					<xs:attribute name="keyValue" type="MFVec3f"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="OrientationInterpolator">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/interp.html#OrientationInterpolator"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DInterpolatorNode">
					<xs:attribute name="keyValue" type="MFRotation"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="PixelTexture">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#PixelTexture"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTexture2DNode">
					<xs:attribute name="image" type="SFImage" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="PlaneSensor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/pointingsensor.html#PlaneSensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DDragSensorNode">
					<xs:attribute name="maxPosition" type="SFVec2f" default="-1 -1"/>
					<xs:attribute name="minPosition" type="SFVec2f" default="0 0"/>
					<xs:attribute name="offset" type="SFVec3f" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="PointLight">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/lighting.html#PointLight"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DLightNode">
					<xs:attribute name="attenuation" type="SFVec3f" default="1 0 0"/>
					<xs:attribute name="location" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="radius" default="100">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="global" type="SFBool" default="true"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="PointSet">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#PointSet"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:group ref="ColorCoordinateContentModel" minOccurs="0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Polyline2D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry2D.html#Polyline2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:attribute name="lineSegments" type="MFVec2f"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Polypoint2D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry2D.html#Polypoint2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:attribute name="point" type="MFVec2f"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="PositionInterpolator">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/interp.html#PositionInterpolator"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DInterpolatorNode">
					<xs:attribute name="keyValue" type="MFVec3f"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="PositionInterpolator2D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/interp.html#PositionInterpolator2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DInterpolatorNode">
					<xs:attribute name="keyValue" type="MFVec2f"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ProximitySensor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/envsensor.html#ProximitySensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DEnvironmentalSensorNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Rectangle2D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry2D.html#Rectangle2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:attribute name="size" type="SFVec2f" default="2 2"/>
					<xs:attribute name="solid" type="SFBool" default="false"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ScalarInterpolator">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/interp.html#ScalarInterpolator"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DInterpolatorNode">
					<xs:attribute name="keyValue" type="MFFloat"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="EaseInEaseOut">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/interp.html#EaseInEaseOut"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChildNode">
					<xs:attribute name="easeInEaseOut" type="MFVec2f"/>
                                        <xs:attribute name="key" type="MFFloat"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="SplinePositionInterpolator">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/interp.html#SplinePositionInterpolator"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DInterpolatorNode">
					<xs:attribute name="closed" type="SFBool" default="false"/>
					<xs:attribute name="keyValue" type="MFVec3f"/>
					<xs:attribute name="keyVelocity" type="MFVec3f"/>
					<xs:attribute name="normalizeVelocity" type="SFBool" default="false"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="SplinePositionInterpolator2D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/interp.html#SplinePositionInterpolator2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DInterpolatorNode">
					<xs:attribute name="closed" type="SFBool" default="false"/>
					<xs:attribute name="keyValue" type="MFVec2f"/>
					<xs:attribute name="keyVelocity" type="MFVec2f"/>
					<xs:attribute name="normalizeVelocity" type="SFBool" default="false"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="SplineScalarInterpolator">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/interp.html#SplineScalarInterpolator"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DInterpolatorNode">
					<xs:attribute name="closed" type="SFBool" default="false"/>
					<xs:attribute name="keyValue" type="MFFloat"/>
					<xs:attribute name="keyVelocity" type="MFFloat"/>
					<xs:attribute name="normalizeVelocity" type="SFBool" default="false"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="SquadOrientationInterpolator">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/interp.html#SquadOrientationInterpolator"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DInterpolatorNode">
					<xs:attribute name="closed" type="SFBool" default="false"/>
					<xs:attribute name="keyValue" type="MFRotation"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Script">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DUrlObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/scripting.html#Script"/>
		</xs:annotation>
		<xs:complexType mixed="true">
			<xs:complexContent>
				<xs:extension base="X3DScriptNode">
					<xs:attribute name="directOutput" type="SFBool" default="false"/>
					<xs:attribute name="mustEvaluate" type="SFBool" default="false"/>
					<xs:attribute name="url" type="MFString"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Shape">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shape.html#Shape"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DShapeNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Sound">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/sound.html#Sound"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DSoundNode">
					<xs:attribute name="direction" type="SFVec3f" default="0 0 1"/>
					<xs:attribute name="intensity" type="intensityType" default="1"/>
					<xs:attribute name="location" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="maxBack" type="SFFloat" default="10"/>
					<xs:attribute name="maxFront" type="SFFloat" default="10"/>
					<xs:attribute name="minBack" type="SFFloat" default="1"/>
					<xs:attribute name="minFront" type="SFFloat" default="1"/>
					<xs:attribute name="priority" type="intensityType" default="0"/>
					<xs:attribute name="spatialize" type="SFBool" default="true"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Sphere">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry3D.html#Sphere"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:attribute name="radius" default="1">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minExclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="solid" type="SFBool" default="true"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="SphereSensor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/pointingsensor.html#SphereSensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DDragSensorNode">
					<xs:attribute name="offset" type="SFRotation" default="0 1 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="SpotLight">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/lighting.html#SpotLight"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DLightNode">
					<xs:attribute name="attenuation" type="SFVec3f" default="1 0 0"/>
					<xs:attribute name="beamWidth" default="0.7854">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minExclusive value="0"/>
								<xs:maxInclusive value="1.570796"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="cutOffAngle" default="1.570796">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minExclusive value="0"/>
								<xs:maxInclusive value="1.570796"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="direction" type="SFVec3f" default="0 0 -1"/>
					<xs:attribute name="location" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="radius" default="100">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="global" type="SFBool" default="true"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="StaticGroup">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/group.html#StaticGroup"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="StringSensor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/keyboard.html#StringSensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DKeyDeviceSensorNode">
					<xs:attribute name="deletionAllowed" type="SFBool" default="true"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Switch">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/group.html#Switch"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode">
					<xs:attribute name="whichChoice" type="SFInt32" default="-1"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Text">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/text.html#Text"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:sequence-cl minOccurs="0">
						<xs:group ref="TextChildContentModel"/>
					</xs:sequence-cl>
					<xs:attribute name="string" type="MFString"/>
					<xs:attribute name="length" type="MFFloat"/>
					<xs:attribute name="maxExtent" default="0.0">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="solid" type="SFBool" default="false"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TextureBackground">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/enveffects.html#TextureBackground"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DBackgroundNode">
					<xs:group ref="TextureBackgroundChildContentModel" minOccurs="0" maxOccurs="6"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TextureCoordinate">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#TextureCoordinate"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTextureCoordinateNode">
					<xs:attribute name="point" type="MFVec2f"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TextureCoordinateGenerator">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#TextureCoordinateGenerator"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTextureCoordinateNode">
					<xs:attribute name="mode" type="SFString" default="SPHERE"/>
					<xs:attribute name="parameter" type="MFFloat"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TextureTransform">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#TextureTransform"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTextureTransform2DNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TimeSensor">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DSensorNode"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/time.html#TimeSensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTimeDependentNode">
					<xs:attribute name="cycleInterval" type="SFTime" default="1.0"/>
					<xs:attribute name="enabled" type="SFBool" default="true"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TimeTrigger">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/utils.html#TimeTrigger"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChildNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TouchSensor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/pointingsensor.html#TouchSensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTouchSensorNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Transform">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/group.html#Transform"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode">
					<xs:attribute name="center" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="rotation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="scale" type="SFVec3f" default="1 1 1"/>
					<xs:attribute name="scaleOrientation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="translation" type="SFVec3f" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TransformSensor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/envsensor.html#TransformSensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DEnvironmentalSensorNode">
					<xs:choice minOccurs="0">
						<xs:annotation>
							<xs:documentation>targetObject</xs:documentation>
						</xs:annotation>
						<xs:element ref="Anchor"/>
						<xs:element ref="Billboard"/>
						<xs:element ref="Collision"/>
						<xs:element ref="Group"/>
						<xs:element ref="LOD"/>
						<xs:element ref="Shape"/>
						<xs:element ref="StaticGroup"/>
						<xs:element ref="Switch"/>
						<xs:element ref="Transform"/>
						<xs:element ref="EspduTransform"/>
						<xs:element ref="ReceiverPdu"/>
						<xs:element ref="SignalPdu"/>
						<xs:element ref="TransmitterPdu"/>
						<xs:element ref="CADAssembly"/>
						<xs:element ref="CADLayer"/>
						<xs:element ref="CADPart"/>
						<xs:element ref="GeoLocation"/>
						<xs:element ref="GeoLOD"/>
						<xs:element ref="GeoTransform"/>
						<xs:element ref="HAnimJoint"/>
						<xs:element ref="HAnimSegment"/>
						<xs:element ref="HAnimSite"/>
						<xs:element ref="PickableGroup"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TriangleFanSet">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#TriangleFanSet"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposedGeometryNode">
					<xs:attribute name="fanCount" type="MFInt32"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TriangleSet">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#TriangleSet"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposedGeometryNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TriangleSet2D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geometry2D.html#TriangleSet2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:attribute name="vertices" type="MFVec2f"/>
					<xs:attribute name="solid" type="SFBool" default="false"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TriangleStripSet">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rendering.html#TriangleStripSet"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposedGeometryNode">
					<xs:attribute name="stripCount" type="MFInt32"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Viewpoint">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/navigation.html#Viewpoint"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DViewpointNode">
					<xs:attribute name="centerOfRotation" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="fieldOfView" type="SFFloat" default="0.7854"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="VisibilitySensor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/envsensor.html#VisibilitySensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DEnvironmentalSensorNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="WorldInfo">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/group.html#WorldInfo"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DInfoNode">
					<xs:attribute name="info" type="MFString"/>
					<xs:attribute name="title" type="SFString"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="component">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#Components"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="SceneGraphStructureNodeType">
					<xs:attribute name="name" type="componentNames" use="required"/>
					<xs:attribute name="level" use="required">
						<xs:simpleType>
							<xs:restriction base="SFInt32">
								<xs:minInclusive value="1"/>
								<xs:maxInclusive value="5"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="unit">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/core.html#UNITStatement"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="SceneGraphStructureNodeType">
					<xs:attribute name="category" type="unitCategories" use="required"/>
					<xs:attribute name="name" type="xs:NMTOKEN" use="required"/>
					<xs:attribute name="conversionFactor" use="required">
						<xs:simpleType>
							<xs:restriction base="SFDouble">
								<xs:minExclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="EXPORT">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#EXPORTSemantics"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="SceneGraphStructureNodeType">
					<xs:attribute name="localDEF" type="xs:IDREF" use="required"/>
					<xs:attribute name="AS" type="xs:NMTOKEN"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="IMPORT">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#IMPORTSemantics"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="SceneGraphStructureNodeType">
					<xs:attribute name="inlineDEF" type="xs:IDREF" use="required"/>
					<xs:attribute name="importedDEF" type="xs:NMTOKEN" use="required"/>
					<xs:attribute name="AS" type="xs:ID"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="IS">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#PROTOdefinitionsemantics"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="SceneGraphStructureNodeType">
					<xs:choice maxOccurs="unbounded">
						<xs:element ref="connect"/>
					</xs:choice>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="connect">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19776-1/V3.2/Part01/concepts.html#IS_ConnectStatementSyntax"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="SceneGraphStructureNodeType">
					<xs:attribute name="nodeField" type="xs:NMTOKEN" use="required"/>
					<xs:attribute name="protoField" type="xs:NMTOKEN" use="required"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="field">
		<xs:annotation>
			<xs:appinfo>field can contain either attribute-value or node content.  field is used by ExternProtoDeclare, ProtoDeclare and Script nodes.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19776-1/V3.2/Part01/concepts.html#NodeAndFieldStatementSyntax"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="SceneGraphStructureNodeType">
					<xs:choice minOccurs="0" maxOccurs="unbounded">
						<xs:group ref="SceneGraphFragmentContentModel"/>
					</xs:choice>
					<xs:attribute name="name" type="xs:NMTOKEN" use="required"/>
					<xs:attribute name="accessType" type="accessTypeNames" use="required"/>
					<xs:attribute name="type" type="fieldTypeName" use="required"/>
					<xs:attribute name="value" type="SFString"/>
					<xs:attribute name="appinfo" type="SFString"/>
					<xs:attribute name="documentation" type="SFString"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="fieldValue">
		<xs:annotation>
			<xs:appinfo>fieldValue can contain either attribute-value or node content.  fieldValue is used by ProtoInstance nodes, reinitializing default values specified in ProtoDeclare field tags.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19776-1/V3.2/Part01/concepts.html#ProtoInstanceAndFieldValueStatement"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="SceneGraphStructureNodeType">
					<xs:choice minOccurs="0" maxOccurs="unbounded">
						<xs:group ref="SceneGraphFragmentContentModel"/>
					</xs:choice>
					<xs:attribute name="name" type="xs:NMTOKEN" use="required"/>
					<xs:attribute name="value" type="SFString"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="head">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19776-1/V3.2/Part01/concepts.html#Header"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="SceneGraphStructureNodeType">
					<xs:sequence-cl>
						<xs:element ref="component" minOccurs="0" maxOccurs="unbounded"/>
						<xs:element ref="unit" minOccurs="0" maxOccurs="unbounded"/>
						<xs:element ref="meta" minOccurs="0" maxOccurs="unbounded"/>
					</xs:sequence-cl>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="meta">
		<xs:annotation>
			<xs:documentation source="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="SceneGraphStructureNodeType">
					<xs:attribute name="name" type="SFString">
						<xs:annotation>
							<xs:documentation>http://www.dublincore.org/documents/dcmi-terms/#terms-description</xs:documentation>
						</xs:annotation>
					</xs:attribute>
					<xs:attribute name="content" type="SFString" use="required">
						<xs:annotation>
							<xs:documentation>http://www.w3.org/TR/html4/struct/global.html#adef-content</xs:documentation>
						</xs:annotation>
					</xs:attribute>
					<xs:attribute name="dir" type="metaDirectionValues">
						<xs:annotation>
							<xs:documentation>http://www.w3.org/TR/html4/struct/dirlang.html#adef-dir</xs:documentation>
						</xs:annotation>
					</xs:attribute>
					<xs:attribute name="http-equiv" type="SFString">
						<xs:annotation>
							<xs:documentation>http://www.w3.org/TR/html4/struct/global.html#adef-http-equiv</xs:documentation>
						</xs:annotation>
					</xs:attribute>
					<xs:attribute name="lang" type="SFString">
						<xs:annotation>
							<xs:documentation>http://www.w3.org/TR/html4/struct/dirlang.html#h-8.1.1</xs:documentation>
						</xs:annotation>
					</xs:attribute>
					<xs:attribute name="scheme" type="SFString">
						<xs:annotation>
							<xs:documentation>http://www.w3.org/TR/html4/struct/global.html#idx-scheme</xs:documentation>
						</xs:annotation>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ExternProtoDeclare">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#Externalprototypesemantics"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DPrototype">
					<xs:sequence-cl minOccurs="0" maxOccurs="unbounded">
						<xs:element ref="field"/>
					</xs:sequence-cl>
					<xs:attribute name="url" type="MFString" use="required"/>
					<xs:attribute name="appinfo" type="SFString"/>
					<xs:attribute name="documentation" type="SFString"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ProtoDeclare">
		<xs:annotation>
			<xs:appinfo>ProtoDeclare defines new Prototype nodes.  Nested ProtoDeclares, ProtoInstances are allowed by specification.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19776-1/V3.2/Part01/concepts.html#PrototypeAndFieldDeclarationSyntax"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DPrototype">
					<xs:sequence-cl>
						<xs:element ref="ProtoInterface" minOccurs="0"/>
						<xs:element ref="ProtoBody"/>
					</xs:sequence-cl>
					<xs:attribute name="appinfo" type="SFString"/>
					<xs:attribute name="documentation" type="SFString"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ProtoInterface">
		<xs:annotation>
			<xs:appinfo>ProtoInterface defines fields for new Prototype nodes.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19776-1/V3.2/Part01/concepts.html#PrototypeAndFieldDeclarationSyntax"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="SceneGraphStructureNodeType">
					<xs:sequence-cl>
						<xs:element ref="field" minOccurs="0" maxOccurs="unbounded"/>
					</xs:sequence-cl>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ProtoBody">
		<xs:annotation>
			<xs:appinfo>ProtoBody contains the definition nodes for new Prototype nodes.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19776-1/V3.2/Part01/concepts.html#PrototypeAndFieldDeclarationSyntax"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="SceneGraphStructureNodeType">
					<xs:sequence-cl>
						<xs:group ref="SceneGraphFragmentWithPrototypeDeclarationsContentModel" maxOccurs="unbounded"/>
					</xs:sequence-cl>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ProtoInstance">
		<xs:annotation>
			<xs:appinfo>Nested ProtoDeclares, ProtoInstances are allowed by specification. ProtoInstance contained content normally captured via fieldValue initializations.
                <xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DNode"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19776-1/V3.2/Part01/concepts.html#ProtoInstanceAndFieldValueStatement"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DPrototype">
					<xs:sequence-cl>
						<xs:element ref="fieldValue" minOccurs="0" maxOccurs="unbounded"/>
						<xs:element ref="IS" minOccurs="0" maxOccurs="unbounded"/>
					</xs:sequence-cl>
					<xs:attributeGroup ref="DEF_USE"/>
					<xs:attributeGroup ref="globalAttributes"/>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="children"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ROUTE">
		<xs:annotation>
			<xs:appinfo>ROUTEs connect event-producing nodes/fields to event-consuming nodes/fields.
                <xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DChildNode"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/concepts.html#ModifyingObjectsRoutes"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="SceneGraphStructureNodeType">
					<xs:attribute name="fromNode" type="xs:IDREF" use="required"/>
					<xs:attribute name="fromField" type="xs:NMTOKEN" use="required"/>
					<xs:attribute name="toNode" type="xs:IDREF" use="required"/>
					<xs:attribute name="toField" type="xs:NMTOKEN" use="required"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Scene">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19776-1/V3.2/Part01/concepts.html#Header"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="SceneGraphStructureNodeType">
					<xs:choice minOccurs="0" maxOccurs="unbounded">
						<xs:group ref="ChildContentModelCore"/>
						<xs:group ref="GroupingNodeChildContentModel"/>
					</xs:choice>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="X3D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19776-1/V3.2/Part01/concepts.html#Header"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="SceneGraphStructureNodeType">
					<xs:sequence-cl>
						<xs:element ref="head" minOccurs="0"/>
						<xs:element ref="Scene"/>
					</xs:sequence-cl>
					<xs:attribute name="version" type="x3dVersion" use="required"/>
					<xs:attribute name="profile" type="profileNames" use="required"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="DISEntityManager">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/dis.html#ReceiverPdu"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChildNode">
					<xs:choice minOccurs="0" maxOccurs="unbounded">
						<xs:annotation>
							<xs:documentation>mapping</xs:documentation>
						</xs:annotation>
						<xs:element ref="DISEntityTypeMapping"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="address" type="SFString" default="localhost"/>
					<xs:attribute name="applicationID" type="SFInt32" default="1"/>
					<xs:attribute name="port" type="SFInt32" default="0"/>
					<xs:attribute name="siteID" type="SFInt32" default="0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="DISEntityTypeMapping">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DChildNode"/>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DUrlObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/dis.html#ReceiverPdu"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNode">
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="mapping"/>
					<xs:attribute name="url" type="MFString"/>
					<xs:attribute name="category" default="0">
						<xs:simpleType>
							<xs:restriction base="SFInt32">
								<xs:minInclusive value="0"/>
								<xs:maxInclusive value="255"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="country" default="0">
						<xs:simpleType>
							<xs:restriction base="SFInt32">
								<xs:minInclusive value="0"/>
								<xs:maxInclusive value="65535"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="domain" default="0">
						<xs:simpleType>
							<xs:restriction base="SFInt32">
								<xs:minInclusive value="0"/>
								<xs:maxInclusive value="255"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="extra" default="0">
						<xs:simpleType>
							<xs:restriction base="SFInt32">
								<xs:minInclusive value="0"/>
								<xs:maxInclusive value="255"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="kind" default="0">
						<xs:simpleType>
							<xs:restriction base="SFInt32">
								<xs:minInclusive value="0"/>
								<xs:maxInclusive value="255"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="specific" default="0">
						<xs:simpleType>
							<xs:restriction base="SFInt32">
								<xs:minInclusive value="0"/>
								<xs:maxInclusive value="255"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="subcategory" default="0">
						<xs:simpleType>
							<xs:restriction base="SFInt32">
								<xs:minInclusive value="0"/>
								<xs:maxInclusive value="255"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="EspduTransform">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DNetworkSensorNode"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/dis.html#EspduTransform"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode">
					<xs:attribute name="enabled" type="SFBool" default="true"/>
					<xs:attribute name="marking" type="SFString"/>
					<xs:attribute name="siteID" type="SFInt32" default="0"/>
					<xs:attribute name="applicationID" type="SFInt32" default="1"/>
					<xs:attribute name="entityID" type="SFInt32" default="0"/>
					<xs:attribute name="forceID" type="SFInt32" default="0"/>
					<xs:attribute name="entityKind" type="SFInt32" default="0"/>
					<xs:attribute name="entityDomain" type="SFInt32" default="0"/>
					<xs:attribute name="entityCountry" type="SFInt32" default="0"/>
					<xs:attribute name="entityCategory" type="SFInt32" default="0"/>
					<xs:attribute name="entitySubCategory" type="SFInt32" default="0"/>
					<xs:attribute name="entitySpecific" type="SFInt32" default="0"/>
					<xs:attribute name="entityExtra" type="SFInt32" default="0"/>
					<xs:attribute name="readInterval" type="SFTime" default="0.1"/>
					<xs:attribute name="writeInterval" type="SFTime" default="1.0"/>
					<xs:attribute name="networkMode" type="networkModeValues" default="standAlone"/>
					<xs:attribute name="translation" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="rotation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="scale" type="SFVec3f" default="1 1 1"/>
					<xs:attribute name="scaleOrientation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="center" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="address" type="SFString" default="localhost"/>
					<xs:attribute name="port" type="SFInt32" default="0"/>
					<xs:attribute name="multicastRelayHost" type="SFString"/>
					<xs:attribute name="multicastRelayPort" type="SFInt32" default="0"/>
					<xs:attribute name="rtpHeaderExpected" type="SFBool" default="false"/>
					<xs:attribute name="deadReckoning" type="SFInt32" default="0"/>
					<xs:attribute name="linearVelocity" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="linearAcceleration" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="fired1" type="SFBool" default="false"/>
					<xs:attribute name="fired2" type="SFBool" default="false"/>
					<xs:attribute name="collisionType" type="SFInt32" default="0"/>
					<xs:attribute name="detonationLocation" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="detonationRelativeLocation" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="detonationResult" type="SFInt32" default="0"/>
					<xs:attribute name="eventApplicationID" type="SFInt32" default="1"/>
					<xs:attribute name="eventEntityID" type="SFInt32" default="0"/>
					<xs:attribute name="eventNumber" type="SFInt32" default="0"/>
					<xs:attribute name="eventSiteID" type="SFInt32" default="0"/>
					<xs:attribute name="munitionStartPoint" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="munitionEndPoint" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="munitionSiteID" type="SFInt32" default="0"/>
					<xs:attribute name="munitionApplicationID" type="SFInt32" default="1"/>
					<xs:attribute name="munitionEntityID" type="SFInt32" default="0"/>
					<xs:attribute name="fireMissionIndex" type="SFInt32" default="0"/>
					<xs:attribute name="warhead" type="SFInt32" default="0"/>
					<xs:attribute name="fuse" type="SFInt32" default="0"/>
					<xs:attribute name="munitionQuantity" type="SFInt32" default="0"/>
					<xs:attribute name="firingRate" type="SFInt32" default="0"/>
					<xs:attribute name="firingRange" type="SFFloat" default="0"/>
					<xs:attribute name="articulationParameterCount" type="SFInt32" default="0"/>
					<xs:attribute name="articulationParameterDesignatorArray" type="MFInt32"/>
					<xs:attribute name="articulationParameterChangeIndicatorArray" type="MFInt32"/>
					<xs:attribute name="articulationParameterIdPartAttachedToArray" type="MFInt32"/>
					<xs:attribute name="articulationParameterTypeArray" type="MFInt32"/>
					<xs:attribute name="articulationParameterArray" type="MFFloat"/>
					<xs:attribute name="geoSystem" type="geoSystemType" default='"GD" "WE"'/>
					<xs:attribute name="geoCoords" type="SFVec3d" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ReceiverPdu">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DBoundedObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/dis.html#ReceiverPdu"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNetworkSensorNode">
					<xs:attribute name="bboxCenter" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="bboxSize" type="boundingBoxSizeType" default="-1 -1 -1"/>
					<xs:attribute name="whichGeometry" type="SFInt32" default="1"/>
					<xs:attribute name="readInterval" type="SFTime" default="0.1"/>
					<xs:attribute name="writeInterval" type="SFTime" default="1.0"/>
					<xs:attribute name="networkMode" type="networkModeValues" default="standAlone"/>
					<xs:attribute name="siteID" type="SFInt32" default="0"/>
					<xs:attribute name="applicationID" type="SFInt32" default="1"/>
					<xs:attribute name="entityID" type="SFInt32" default="0"/>
					<xs:attribute name="address" type="SFString" default="localhost"/>
					<xs:attribute name="port" type="SFInt32" default="0"/>
					<xs:attribute name="multicastRelayHost" type="SFString"/>
					<xs:attribute name="multicastRelayPort" type="SFInt32" default="0"/>
					<xs:attribute name="rtpHeaderExpected" type="SFBool" default="false"/>
					<xs:attribute name="radioID" type="SFInt32" default="0"/>
					<xs:attribute name="receivedPower" type="SFFloat" default="0.0"/>
					<xs:attribute name="receiverState" type="SFInt32" default="0"/>
					<xs:attribute name="transmitterSiteID" type="SFInt32" default="0"/>
					<xs:attribute name="transmitterApplicationID" type="SFInt32" default="0"/>
					<xs:attribute name="transmitterEntityID" type="SFInt32" default="0"/>
					<xs:attribute name="transmitterRadioID" type="SFInt32" default="0"/>
					<xs:attribute name="geoSystem" type="geoSystemType" default='"GD" "WE"'/>
					<xs:attribute name="geoCoords" type="SFVec3d" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="SignalPdu">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DBoundedObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/dis.html#SignalPdu"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNetworkSensorNode">
					<xs:attribute name="bboxCenter" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="bboxSize" type="boundingBoxSizeType" default="-1 -1 -1"/>
					<xs:attribute name="whichGeometry" type="SFInt32" default="1"/>
					<xs:attribute name="readInterval" type="SFTime" default="0.1"/>
					<xs:attribute name="writeInterval" type="SFTime" default="1.0"/>
					<xs:attribute name="networkMode" type="networkModeValues" default="standAlone"/>
					<xs:attribute name="siteID" type="SFInt32" default="0"/>
					<xs:attribute name="applicationID" type="SFInt32" default="1"/>
					<xs:attribute name="entityID" type="SFInt32" default="0"/>
					<xs:attribute name="address" type="SFString" default="localhost"/>
					<xs:attribute name="port" type="SFInt32" default="0"/>
					<xs:attribute name="multicastRelayHost" type="SFString"/>
					<xs:attribute name="multicastRelayPort" type="SFInt32" default="0"/>
					<xs:attribute name="rtpHeaderExpected" type="SFBool" default="false"/>
					<xs:attribute name="radioID" type="SFInt32" default="0"/>
					<xs:attribute name="encodingScheme" type="SFInt32" default="0"/>
					<xs:attribute name="tdlType" type="SFInt32" default="0"/>
					<xs:attribute name="sampleRate" type="SFInt32" default="0"/>
					<xs:attribute name="samples" type="SFInt32" default="0"/>
					<xs:attribute name="dataLength" type="SFInt32" default="0"/>
					<xs:attribute name="data" type="MFInt32"/>
					<xs:attribute name="geoSystem" type="geoSystemType" default='"GD" "WE"'/>
					<xs:attribute name="geoCoords" type="SFVec3d" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TransmitterPdu">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DBoundedObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/dis.html#TransmitterPdu"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNetworkSensorNode">
					<xs:attribute name="bboxCenter" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="bboxSize" type="boundingBoxSizeType" default="-1 -1 -1"/>
					<xs:attribute name="whichGeometry" type="SFInt32" default="1"/>
					<xs:attribute name="readInterval" type="SFTime" default="0.1"/>
					<xs:attribute name="writeInterval" type="SFTime" default="1.0"/>
					<xs:attribute name="networkMode" type="networkModeValues" default="standAlone"/>
					<xs:attribute name="siteID" type="SFInt32" default="0"/>
					<xs:attribute name="applicationID" type="SFInt32" default="1"/>
					<xs:attribute name="entityID" type="SFInt32" default="0"/>
					<xs:attribute name="address" type="SFString" default="localhost"/>
					<xs:attribute name="port" type="SFInt32" default="0"/>
					<xs:attribute name="multicastRelayHost" type="SFString"/>
					<xs:attribute name="multicastRelayPort" type="SFInt32" default="0"/>
					<xs:attribute name="rtpHeaderExpected" type="SFBool" default="false"/>
					<xs:attribute name="radioID" type="SFInt32" default="0"/>
					<xs:attribute name="antennaLocation" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="antennaPatternLength" type="SFInt32" default="0"/>
					<xs:attribute name="antennaPatternType" type="SFInt32" default="0"/>
					<xs:attribute name="cryptoKeyID" type="SFInt32" default="0"/>
					<xs:attribute name="cryptoSystem" type="SFInt32" default="0"/>
					<xs:attribute name="frequency" type="SFInt32" default="0"/>
					<xs:attribute name="inputSource" type="SFInt32" default="0"/>
					<xs:attribute name="lengthOfModulationParameters" type="SFInt32" default="0"/>
					<xs:attribute name="modulationTypeDetail" type="SFInt32" default="0"/>
					<xs:attribute name="modulationTypeMajor" type="SFInt32" default="0"/>
					<xs:attribute name="modulationTypeSpreadSpectrum" type="SFInt32" default="0"/>
					<xs:attribute name="modulationTypeSystem" type="SFInt32" default="0"/>
					<xs:attribute name="power" type="SFFloat" default="0.0"/>
					<xs:attribute name="radioEntityTypeCategory" type="SFInt32" default="0"/>
					<xs:attribute name="radioEntityTypeCountry" type="SFInt32" default="0"/>
					<xs:attribute name="radioEntityTypeDomain" type="SFInt32" default="0"/>
					<xs:attribute name="radioEntityTypeKind" type="SFInt32" default="0"/>
					<xs:attribute name="radioEntityTypeNomenclature" type="SFInt32" default="0"/>
					<xs:attribute name="radioEntityTypeNomenclatureVersion" type="SFInt32" default="0"/>
					<xs:attribute name="relativeAntennaLocation" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="transmitFrequencyBandwidth" type="SFFloat" default="0"/>
					<xs:attribute name="transmitState" type="SFInt32" default="0"/>
					<xs:attribute name="geoSystem" type="geoSystemType" default='"GD" "WE"'/>
					<xs:attribute name="geoCoords" type="SFVec3d" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:simpleType name="geoSystemType">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geodata.html#Specifyingaspatialreference"/>
		</xs:annotation>
		<xs:restriction base="SFString"/>
	</xs:simpleType>
	<xs:element name="GeoCoordinate">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geodata.html#GeoCoordinate"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DCoordinateNode">
					<xs:sequence-cl minOccurs="0">
						<xs:element ref="GeoOrigin"/>
					</xs:sequence-cl>
					<xs:attribute name="geoSystem" type="geoSystemType" default='"GD" "WE"'/>
					<xs:attribute name="point" type="MFVec3d"/>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="coord"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="GeoElevationGrid">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geodata.html#GeoElevationGrid"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometryNode">
					<xs:choice minOccurs="0">
						<xs:sequence-cl>
							<xs:element ref="GeoOrigin"/>
							<xs:group ref="ColorNormalTexCoordContentModel" minOccurs="0"/>
						</xs:sequence-cl>
						<xs:sequence-cl minOccurs="0">
							<xs:group ref="ColorNormalTexCoordContentModel"/>
							<xs:element ref="GeoOrigin" minOccurs="0"/>
						</xs:sequence-cl>
					</xs:choice>
					<xs:attribute name="geoSystem" type="geoSystemType" default='"GD" "WE"'/>
					<xs:attribute name="geoGridOrigin" type="SFVec3d" default="0 0 0"/>
					<xs:attribute name="height" type="MFDouble" default="0 0"/>
					<xs:attribute name="ccw" type="SFBool" default="true"/>
					<xs:attribute name="colorPerVertex" type="SFBool" default="true"/>
					<xs:attribute name="creaseAngle" default="0">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="normalPerVertex" type="SFBool" default="true"/>
					<xs:attribute name="solid" type="SFBool" default="true"/>
					<xs:attribute name="xDimension" type="SFInt32" default="0"/>
					<xs:attribute name="xSpacing" default="1.0">
						<xs:simpleType>
							<xs:restriction base="SFDouble">
								<xs:minInclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="yScale" default="1">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="zDimension" type="SFInt32" default="0"/>
					<xs:attribute name="zSpacing" default="1.0">
						<xs:simpleType>
							<xs:restriction base="SFDouble">
								<xs:minInclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="GeoLocation">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geodata.html#GeoLocation"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode">
					<xs:attribute name="geoSystem" type="geoSystemType" default='"GD" "WE"'/>
					<xs:attribute name="geoCoords" type="SFVec3d" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="GeoLOD">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geodata.html#GeoLOD"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode">
					<xs:attribute name="geoSystem" type="geoSystemType" default='"GD" "WE"'/>
					<xs:attribute name="rootUrl" type="MFString"/>
					<xs:attribute name="child1Url" type="MFString"/>
					<xs:attribute name="child2Url" type="MFString"/>
					<xs:attribute name="child3Url" type="MFString"/>
					<xs:attribute name="child4Url" type="MFString"/>
					<xs:attribute name="center" type="SFVec3d" default="0 0 0"/>
					<xs:attribute name="range" type="SFFloat" default="10"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="GeoMetadata">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geodata.html#GeoMetadata"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DInfoNode">
					<xs:choice minOccurs="0">
						<xs:element ref="GeoCoordinate"/>
						<xs:element ref="GeoElevationGrid"/>
						<xs:element ref="GeoLocation"/>
						<xs:element ref="GeoOrigin"/>
						<xs:element ref="GeoLOD"/>
						<xs:element ref="GeoPositionInterpolator"/>
						<xs:element ref="GeoProximitySensor"/>
						<xs:element ref="GeoTouchSensor"/>
						<xs:element ref="GeoTransform"/>
						<xs:element ref="GeoViewpoint"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="url" type="MFString"/>
					<xs:attribute name="summary" type="MFString"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="GeoOrigin">
		<xs:annotation>
			<xs:appinfo>GeoOrigin is deprecated and discouraged (but nevertheless allowed) in X3D v3.3.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geodata.html#GeoOrigin"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNode">
					<xs:attribute name="geoSystem" type="geoSystemType" default='"GD" "WE"'/>
					<xs:attribute name="geoCoords" type="SFVec3d" default="0 0 0"/>
					<xs:attribute name="rotateYUp" type="SFBool" default="false"/>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="geoOrigin"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="GeoPositionInterpolator">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geodata.html#GeoPositionInterpolator"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DInterpolatorNode">
					<xs:sequence-cl minOccurs="0">
						<xs:element ref="GeoOrigin"/>
					</xs:sequence-cl>
					<xs:attribute name="geoSystem" type="geoSystemType" default='"GD" "WE"'/>
					<xs:attribute name="keyValue" type="MFVec3d"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="GeoProximitySensor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/x3d/specifications/ISO-IEC-19775-X3DAbstractSpecification_Revision1_to_Part1/Part01/components/geodata.html#GeoProximitySensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DEnvironmentalSensorNode">
					<xs:sequence-cl minOccurs="0">
						<xs:element ref="GeoOrigin"/>
					</xs:sequence-cl>
					<xs:attribute name="geoSystem" type="geoSystemType" default='"GD" "WE"'/>
					<xs:attribute name="geoCenter" type="SFVec3d" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="GeoTouchSensor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geodata.html#GeoTouchSensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTouchSensorNode">
					<xs:sequence-cl minOccurs="0">
						<xs:element ref="GeoOrigin"/>
					</xs:sequence-cl>
					<xs:attribute name="geoSystem" type="geoSystemType" default='"GD" "WE"'/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="GeoTransform">
		<xs:annotation>
			<xs:documentation/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode">
					<xs:attribute name="geoCenter" type="SFVec3d" default="0 0 0"/>
					<xs:attribute name="geoSystem" type="geoSystemType" default='"GD" "WE"'/>
					<xs:attribute name="rotation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="scale" type="SFVec3f" default="1 1 1"/>
					<xs:attribute name="scaleOrientation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="translation" type="SFVec3f" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="GeoViewpoint">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/geodata.html#GeoViewpoint"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DBindableNode">
					<xs:sequence-cl minOccurs="0">
						<xs:element ref="GeoOrigin"/>
					</xs:sequence-cl>
					<xs:attribute name="centerOfRotation" type="SFVec3d" default="0 0 0"/>
					<xs:attribute name="description" type="SFString"/>
					<xs:attribute name="geoSystem" type="geoSystemType" default='"GD" "WE"'/>
					<xs:attribute name="fieldOfView" type="SFFloat" default="0.7854"/>
					<xs:attribute name="jump" type="SFBool" default="true"/>
					<xs:attribute name="orientation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="position" type="SFVec3d" default="0 0 100000"/>
					<xs:attribute name="retainUserOffsets" type="SFBool" default="false"/>
					<xs:attribute name="speedFactor" type="SFFloat" default="1.0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:simpleType name="jointName">
		<xs:annotation>
			<xs:documentation source="http://ecetemp.uwaterloo.ca/~h-anim/spec1.1/#hierarchy"/>
		</xs:annotation>
		<xs:restriction base="xs:string">
			<xs:enumeration value="c1"/>
			<xs:enumeration value="c2"/>
			<xs:enumeration value="c3"/>
			<xs:enumeration value="c4"/>
			<xs:enumeration value="c5"/>
			<xs:enumeration value="c6"/>
			<xs:enumeration value="c7"/>
			<xs:enumeration value="HumanoidRoot"/>
			<xs:enumeration value="jaw"/>
			<xs:enumeration value="l_acromioclavicular"/>
			<xs:enumeration value="l_ankle"/>
			<xs:enumeration value="l_calf"/>
			<xs:enumeration value="l_clavicle"/>
			<xs:enumeration value="l_elbow"/>
			<xs:enumeration value="l_eyeball"/>
			<xs:enumeration value="l_eyeball_joint"/>
			<xs:enumeration value="l_eyebrow"/>
			<xs:enumeration value="l_eyebrow_joint"/>
			<xs:enumeration value="l_eyelid"/>
			<xs:enumeration value="l_eyelid_joint"/>
			<xs:enumeration value="l_forearm"/>
			<xs:enumeration value="l_forefoot"/>
			<xs:enumeration value="l_hand"/>
			<xs:enumeration value="l_hindfoot"/>
			<xs:enumeration value="l_hip"/>
			<xs:enumeration value="l_index_distal"/>
			<xs:enumeration value="l_index_metacarpal"/>
			<xs:enumeration value="l_index_middle"/>
			<xs:enumeration value="l_index_proximal"/>
			<xs:enumeration value="l_index0"/>
			<xs:enumeration value="l_index1"/>
			<xs:enumeration value="l_index2"/>
			<xs:enumeration value="l_index3"/>
			<xs:enumeration value="l_knee"/>
			<xs:enumeration value="l_metatarsal"/>
			<xs:enumeration value="l_middistal"/>
			<xs:enumeration value="l_middle_distal"/>
			<xs:enumeration value="l_middle_metacarpal"/>
			<xs:enumeration value="l_middle_middle"/>
			<xs:enumeration value="l_middle_proximal"/>
			<xs:enumeration value="l_middle0"/>
			<xs:enumeration value="l_middle1"/>
			<xs:enumeration value="l_middle2"/>
			<xs:enumeration value="l_middle3"/>
			<xs:enumeration value="l_midproximal"/>
			<xs:enumeration value="l_midtarsal"/>
			<xs:enumeration value="l_pinky_distal"/>
			<xs:enumeration value="l_pinky_metacarpal"/>
			<xs:enumeration value="l_pinky_middle"/>
			<xs:enumeration value="l_pinky_proximal"/>
			<xs:enumeration value="l_pinky0"/>
			<xs:enumeration value="l_pinky1"/>
			<xs:enumeration value="l_pinky2"/>
			<xs:enumeration value="l_pinky3"/>
			<xs:enumeration value="l_ring_distal"/>
			<xs:enumeration value="l_ring_metacarpal"/>
			<xs:enumeration value="l_ring_middle"/>
			<xs:enumeration value="l_ring_proximal"/>
			<xs:enumeration value="l_ring0"/>
			<xs:enumeration value="l_ring1"/>
			<xs:enumeration value="l_ring2"/>
			<xs:enumeration value="l_ring3"/>
			<xs:enumeration value="l_scapula"/>
			<xs:enumeration value="l_shoulder"/>
			<xs:enumeration value="l_sternoclavicular"/>
			<xs:enumeration value="l_subtalar"/>
			<xs:enumeration value="l_thigh"/>
			<xs:enumeration value="l_thumb_distal"/>
			<xs:enumeration value="l_thumb_metacarpal"/>
			<xs:enumeration value="l_thumb_proximal"/>
			<xs:enumeration value="l_thumb1"/>
			<xs:enumeration value="l_thumb2"/>
			<xs:enumeration value="l_thumb3"/>
			<xs:enumeration value="l_upperarm"/>
			<xs:enumeration value="l_wrist"/>
			<xs:enumeration value="l1"/>
			<xs:enumeration value="l2"/>
			<xs:enumeration value="l3"/>
			<xs:enumeration value="l4"/>
			<xs:enumeration value="l5"/>
			<xs:enumeration value="pelvis"/>
			<xs:enumeration value="r_acromioclavicular"/>
			<xs:enumeration value="r_ankle"/>
			<xs:enumeration value="r_calf"/>
			<xs:enumeration value="r_clavicle"/>
			<xs:enumeration value="r_elbow"/>
			<xs:enumeration value="r_eyeball"/>
			<xs:enumeration value="r_eyeball_joint"/>
			<xs:enumeration value="r_eyebrow"/>
			<xs:enumeration value="r_eyebrow_joint"/>
			<xs:enumeration value="r_eyelid"/>
			<xs:enumeration value="r_eyelid_joint"/>
			<xs:enumeration value="r_forearm"/>
			<xs:enumeration value="r_forefoot"/>
			<xs:enumeration value="r_hand"/>
			<xs:enumeration value="r_hindfoot"/>
			<xs:enumeration value="r_hip"/>
			<xs:enumeration value="r_index_distal"/>
			<xs:enumeration value="r_index_metacarpal"/>
			<xs:enumeration value="r_index_middle"/>
			<xs:enumeration value="r_index_proximal"/>
			<xs:enumeration value="r_index0"/>
			<xs:enumeration value="r_index1"/>
			<xs:enumeration value="r_index2"/>
			<xs:enumeration value="r_index3"/>
			<xs:enumeration value="r_knee"/>
			<xs:enumeration value="r_metatarsal"/>
			<xs:enumeration value="r_middistal"/>
			<xs:enumeration value="r_middle_distal"/>
			<xs:enumeration value="r_middle_metacarpal"/>
			<xs:enumeration value="r_middle_middle"/>
			<xs:enumeration value="r_middle_proximal"/>
			<xs:enumeration value="r_middle0"/>
			<xs:enumeration value="r_middle1"/>
			<xs:enumeration value="r_middle2"/>
			<xs:enumeration value="r_middle3"/>
			<xs:enumeration value="r_midproximal"/>
			<xs:enumeration value="r_midtarsal"/>
			<xs:enumeration value="r_pinky_distal"/>
			<xs:enumeration value="r_pinky_metacarpal"/>
			<xs:enumeration value="r_pinky_middle"/>
			<xs:enumeration value="r_pinky_proximal"/>
			<xs:enumeration value="r_pinky0"/>
			<xs:enumeration value="r_pinky1"/>
			<xs:enumeration value="r_pinky2"/>
			<xs:enumeration value="r_pinky3"/>
			<xs:enumeration value="r_ring_distal"/>
			<xs:enumeration value="r_ring_metacarpal"/>
			<xs:enumeration value="r_ring_middle"/>
			<xs:enumeration value="r_ring_proximal"/>
			<xs:enumeration value="r_ring0"/>
			<xs:enumeration value="r_ring1"/>
			<xs:enumeration value="r_ring2"/>
			<xs:enumeration value="r_ring3"/>
			<xs:enumeration value="r_scapula"/>
			<xs:enumeration value="r_shoulder"/>
			<xs:enumeration value="r_sternoclavicular"/>
			<xs:enumeration value="r_subtalar"/>
			<xs:enumeration value="r_thigh"/>
			<xs:enumeration value="r_thumb_distal"/>
			<xs:enumeration value="r_thumb_metacarpal"/>
			<xs:enumeration value="r_thumb_proximal"/>
			<xs:enumeration value="r_thumb1"/>
			<xs:enumeration value="r_thumb2"/>
			<xs:enumeration value="r_thumb3"/>
			<xs:enumeration value="r_upperarm"/>
			<xs:enumeration value="r_wrist"/>
			<xs:enumeration value="sacroiliac"/>
			<xs:enumeration value="sacrum"/>
			<xs:enumeration value="skull"/>
			<xs:enumeration value="skullbase"/>
			<xs:enumeration value="t1"/>
			<xs:enumeration value="t2"/>
			<xs:enumeration value="t3"/>
			<xs:enumeration value="t4"/>
			<xs:enumeration value="t5"/>
			<xs:enumeration value="t6"/>
			<xs:enumeration value="t7"/>
			<xs:enumeration value="t8"/>
			<xs:enumeration value="t9"/>
			<xs:enumeration value="t10"/>
			<xs:enumeration value="t11"/>
			<xs:enumeration value="t12"/>
			<xs:enumeration value="temporomandibular"/>
			<xs:enumeration value="vc1"/>
			<xs:enumeration value="vc2"/>
			<xs:enumeration value="vc3"/>
			<xs:enumeration value="vc4"/>
			<xs:enumeration value="vc5"/>
			<xs:enumeration value="vc6"/>
			<xs:enumeration value="vc7"/>
			<xs:enumeration value="vl1"/>
			<xs:enumeration value="vl2"/>
			<xs:enumeration value="vl3"/>
			<xs:enumeration value="vl4"/>
			<xs:enumeration value="vl5"/>
			<xs:enumeration value="vt1"/>
			<xs:enumeration value="vt2"/>
			<xs:enumeration value="vt3"/>
			<xs:enumeration value="vt4"/>
			<xs:enumeration value="vt5"/>
			<xs:enumeration value="vt6"/>
			<xs:enumeration value="vt7"/>
			<xs:enumeration value="vt8"/>
			<xs:enumeration value="vt9"/>
			<xs:enumeration value="vt10"/>
			<xs:enumeration value="vt11"/>
			<xs:enumeration value="vt12"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:element name="HAnimDisplacer">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/hanim.html#HAnimDisplacer"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGeometricPropertyNode">
					<xs:attribute name="name" type="SFString"/>
					<xs:attribute name="coordIndex" type="MFInt32"/>
					<xs:attribute name="displacements" type="MFVec3f"/>
					<xs:attribute name="weight" type="SFFloat" default="0.0"/>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="displacers"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="HAnimHumanoid">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DBoundedObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/hanim.html#HAnimHumanoid"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DHumanoidNode">
					<xs:sequence-cl minOccurs="0" maxOccurs="unbounded">
						<xs:group ref="ChildContentModel" minOccurs="0" maxOccurs="unbounded"/>
						<xs:choice minOccurs="0">
							<xs:element ref="Coordinate"/>
							<xs:element ref="CoordinateDouble"/>
						</xs:choice>
						<xs:element ref="Normal" minOccurs="0"/>
					</xs:sequence-cl>
					<xs:attribute name="name" type="SFString"/>
					<xs:attribute name="center" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="rotation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="scale" type="SFVec3f" default="1 1 1"/>
					<xs:attribute name="scaleOrientation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="translation" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="info" type="MFString"/>
					<xs:attribute name="version" type="SFString"/>
					<xs:attribute name="bboxCenter" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="bboxSize" type="boundingBoxSizeType" default="-1 -1 -1"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="HAnimJoint">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/hanim.html#HAnimJoint"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode">
					<xs:attribute name="name" type="jointName"/>
					<xs:attribute name="center" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="rotation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="scale" type="SFVec3f" default="1 1 1"/>
					<xs:attribute name="scaleOrientation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="translation" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="skinCoordIndex" type="MFInt32"/>
					<xs:attribute name="skinCoordWeight" type="MFFloat"/>
					<xs:attribute name="llimit" type="MFFloat"/>
					<xs:attribute name="ulimit" type="MFFloat"/>
					<xs:attribute name="limitOrientation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="stiffness" type="MFFloat" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="HAnimSegment">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/hanim.html#HAnimSegment"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode">
					<xs:sequence-cl minOccurs="0" maxOccurs="unbounded">
						<xs:element ref="HAnimDisplacer"/>
					</xs:sequence-cl>
					<xs:attribute name="name" type="SFString"/>
					<xs:attribute name="mass" type="SFFloat" default="0"/>
					<xs:attribute name="centerOfMass" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="momentsOfInertia" type="MFFloat" default="0 0 0 0 0 0 0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="HAnimSite">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/hanim.html#HAnimSite"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode">
					<xs:attribute name="name" type="SFString"/>
					<xs:attribute name="center" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="rotation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="scale" type="SFVec3f" default="1 1 1"/>
					<xs:attribute name="scaleOrientation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="translation" type="SFVec3f" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Contour2D">
		<xs:annotation>
			<xs:appinfo>http://www.blaxxun.com/support/developerguide/developer/contact/3d/nurbs/spec/nodes.htm#Contour2D</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html#Contour2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNode">
					<xs:choice minOccurs="0" maxOccurs="unbounded">
						<xs:element ref="NurbsCurve2D"/>
						<xs:element ref="ContourPolyline2D"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="trimmingContour"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ContourPolyline2D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html#ContourPolyline2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNurbsControlCurveNode">
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="children"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="NurbsCurve">
		<xs:annotation>
			<xs:appinfo>http://www.blaxxun.com/support/developerguide/developer/contact/3d/nurbs/spec/nodes.htm#NurbsCurve</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html#NurbsCurve"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DParametricGeometryNode">
					<xs:choice minOccurs="0">
						<xs:element ref="Coordinate"/>
						<xs:element ref="CoordinateDouble"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="closed" type="SFBool" default="false"/>
					<xs:attribute name="knot" type="MFDouble"/>
					<xs:attribute name="order" type="SFInt32" default="3"/>
					<xs:attribute name="tessellation" type="SFInt32" default="0"/>
					<xs:attribute name="weight" type="MFDouble"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="NurbsCurve2D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html#NurbsCurve2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNurbsControlCurveNode">
					<xs:attribute name="closed" type="SFBool" default="false"/>
					<xs:attribute name="knot" type="MFDouble"/>
					<xs:attribute name="order" type="SFInt32" default="3"/>
					<xs:attribute name="tessellation" type="SFInt32" default="0"/>
					<xs:attribute name="weight" type="MFDouble"/>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="children"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="NurbsOrientationInterpolator">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html#NurbsOrientationInterpolator"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChildNode">
					<xs:choice minOccurs="0">
						<xs:element ref="Coordinate"/>
						<xs:element ref="CoordinateDouble"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="knot" type="MFDouble"/>
					<xs:attribute name="order" type="SFInt32" default="3"/>
					<xs:attribute name="weight" type="MFDouble"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="NurbsPatchSurface">
		<xs:annotation>
			<xs:documentation source="http://www.blaxxun.com/support/developerguide/developer/contact/3d/nurbs/spec/nodes.htm#NurbsSurface"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNurbsSurfaceGeometryNode">
					<xs:choice minOccurs="0" maxOccurs="2">
						<xs:element ref="Coordinate"/>
						<xs:element ref="CoordinateDouble"/>
						<xs:element ref="TextureCoordinate"/>
						<xs:element ref="TextureCoordinateGenerator"/>
						<xs:element ref="NurbsTextureCoordinate"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="NurbsPositionInterpolator">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html#NurbsPositionInterpolator"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChildNode">
					<xs:choice minOccurs="0">
						<xs:element ref="Coordinate"/>
						<xs:element ref="CoordinateDouble"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="knot" type="MFDouble"/>
					<xs:attribute name="order" type="SFInt32" default="3"/>
					<xs:attribute name="weight" type="MFDouble"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="NurbsSet">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DBoundedObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html#NurbsSet"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChildNode">
					<xs:sequence-cl minOccurs="0" maxOccurs="unbounded">
						<xs:element ref="NurbsPatchSurface"/>
						<xs:element ref="NurbsSweptSurface"/>
						<xs:element ref="NurbsSwungSurface"/>
						<xs:element ref="NurbsTrimmedSurface"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:sequence-cl>
					<xs:attribute name="tessellationScale" type="SFFloat" default="1.0"/>
					<xs:attribute name="bboxCenter" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="bboxSize" type="boundingBoxSizeType" default="-1 -1 -1"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="NurbsSurfaceInterpolator">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html#NurbsSurfaceInterpolator"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChildNode">
					<xs:choice minOccurs="0">
						<xs:element ref="Coordinate"/>
						<xs:element ref="CoordinateDouble"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="uDimension" type="SFInt32" default="0"/>
					<xs:attribute name="vDimension" type="SFInt32" default="0"/>
					<xs:attribute name="uKnot" type="MFDouble"/>
					<xs:attribute name="vKnot" type="MFDouble"/>
					<xs:attribute name="uOrder" type="SFInt32" default="3"/>
					<xs:attribute name="vOrder" type="SFInt32" default="3"/>
					<xs:attribute name="weight" type="MFDouble"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="NurbsSweptSurface">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html#NurbsSweptSurface"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DParametricGeometryNode">
					<xs:choice minOccurs="0" maxOccurs="2">
						<xs:element ref="ContourPolyline2D"/>
						<xs:element ref="NurbsCurve2D"/>
						<xs:element ref="NurbsCurve"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="ccw" type="SFBool" default="true"/>
					<xs:attribute name="solid" type="SFBool" default="true"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="NurbsSwungSurface">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html#NurbsSwungSurface"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DParametricGeometryNode">
					<xs:choice minOccurs="0" maxOccurs="2">
						<xs:element ref="ContourPolyline2D"/>
						<xs:element ref="NurbsCurve2D"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="ccw" type="SFBool" default="true"/>
					<xs:attribute name="solid" type="SFBool" default="true"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="NurbsTextureCoordinate">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html#NurbsTextureCoordinate"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTextureCoordinateNode">
					<xs:attribute name="controlPoint" type="MFVec2f"/>
					<xs:attribute name="uDimension" type="SFInt32" default="0"/>
					<xs:attribute name="vDimension" type="SFInt32" default="0"/>
					<xs:attribute name="uKnot" type="MFDouble"/>
					<xs:attribute name="vKnot" type="MFDouble"/>
					<xs:attribute name="uOrder" type="SFInt32" default="3"/>
					<xs:attribute name="vOrder" type="SFInt32" default="3"/>
					<xs:attribute name="weight" type="MFFloat"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="NurbsTrimmedSurface">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/nurbs.html#NurbsTrimmedSurface"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNurbsSurfaceGeometryNode">
					<xs:choice minOccurs="0" maxOccurs="unbounded">
						<xs:element ref="Contour2D"/>
						<xs:element ref="Coordinate"/>
						<xs:element ref="CoordinateDouble"/>
						<xs:element ref="TextureCoordinate"/>
						<xs:element ref="TextureCoordinateGenerator"/>
						<xs:element ref="NurbsTextureCoordinate"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="CADAssembly">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DGroupingNode"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/CADGeometry.html#CADAssembly"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DProductStructureChildNode">
					<xs:choice minOccurs="0" maxOccurs="unbounded">
						<xs:element ref="CADAssembly">
							<xs:annotation>
								<xs:documentation>subassembly</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="CADPart"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="Anchor"/>
						<xs:element ref="Billboard"/>
						<xs:element ref="Collision"/>
						<xs:element ref="Group"/>
						<xs:element ref="Inline"/>
						<xs:element ref="LOD"/>
						<xs:element ref="Transform"/>
						<xs:element ref="DirectionalLight"/>
						<xs:element ref="NavigationInfo"/>
						<xs:element ref="OrthoViewpoint"/>
						<xs:element ref="Viewpoint"/>
						<xs:element ref="ViewpointGroup"/>
						<xs:element ref="WorldInfo"/>
					</xs:choice>
					<xs:attribute name="bboxCenter" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="bboxSize" type="boundingBoxSizeType" default="-1 -1 -1"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="CADFace">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DBoundedObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/CADGeometry.html#CADFace "/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DProductStructureChildNode">
					<xs:choice minOccurs="0">
						<xs:element ref="Shape"/>
						<xs:element ref="LOD"/>
						<xs:element ref="Transform"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="bboxCenter" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="bboxSize" type="boundingBoxSizeType" default="-1 -1 -1"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="CADLayer">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/CADGeometry.html#CADLayer "/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode">
					<xs:attribute name="name" type="SFString"/>
					<xs:attribute name="visible" type="MFBool"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="CADPart">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DGroupingNode"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/CADGeometry.html#CADPart "/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DProductStructureChildNode">
					<xs:choice minOccurs="0" maxOccurs="unbounded">
						<xs:element ref="CADFace"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="center" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="rotation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="scale" type="SFVec3f" default="1 1 1"/>
					<xs:attribute name="scaleOrientation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="translation" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="bboxCenter" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="bboxSize" type="boundingBoxSizeType" default="-1 -1 -1"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="IndexedQuadSet">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/CADGeometry.html#IndexedQuadSet"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposedGeometryNode">
					<xs:attribute name="index" type="MFInt32"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="QuadSet">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/CADGeometry.html#QuadSet"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposedGeometryNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ComposedShader">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DShaderNode"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shaders.html#ComposedShader"/>
		</xs:annotation>
		<xs:complexType mixed="true">
			<xs:complexContent>
				<xs:extension base="X3DProgrammableShaderObject">
					<xs:sequence-cl minOccurs="0" maxOccurs="unbounded">
						<xs:annotation>
							<xs:documentation>parts</xs:documentation>
						</xs:annotation>
						<xs:choice>
                                                        <xs:element ref="ShaderPart"/>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:choice>
					</xs:sequence-cl>
					<xs:attribute name="language" type="SFString"/>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="shaders">
						<xs:annotation>
							<xs:documentation>parent Appearance node</xs:documentation>
						</xs:annotation>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="FloatVertexAttribute">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shaders.html#FloatVertexAttribute"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DVertexAttributeNode">
					<xs:attribute name="value" type="MFFloat"/>
					<xs:attribute name="numComponents" default="4">
						<xs:simpleType>
							<xs:restriction base="SFInt32">
								<xs:minInclusive value="1"/>
								<xs:maxInclusive value="4"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Matrix3VertexAttribute">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shaders.html#Matrix3VertexAttribute"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DVertexAttributeNode">
					<xs:attribute name="value" type="MFMatrix3f"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Matrix4VertexAttribute">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shaders.html#Matrix4VertexAttribute"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DVertexAttributeNode">
					<xs:attribute name="value" type="MFMatrix4f"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="PackagedShader">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DShaderNode,X3DUrlObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shaders.html#PackagedShader"/>
		</xs:annotation>
		<xs:complexType mixed="true">
			<xs:complexContent>
				<xs:extension base="X3DProgrammableShaderObject">
					<xs:attribute name="language" type="SFString"/>
					<xs:attribute name="url" type="MFString"/>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="shaders">
						<xs:annotation>
							<xs:documentation>parent Appearance node</xs:documentation>
						</xs:annotation>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ProgramShader">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shaders.html#ProgramShader"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DShaderNode">
					<xs:sequence-cl minOccurs="0" maxOccurs="unbounded">
						<xs:annotation>
							<xs:documentation>programs</xs:documentation>
						</xs:annotation>
						<xs:element ref="ShaderProgram"/>
					</xs:sequence-cl>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ShaderPart">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DUrlObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shaders.html#ShaderPart"/>
		</xs:annotation>
		<xs:complexType mixed="true">
			<xs:complexContent>
				<xs:extension base="X3DNodeMixedContent">
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="parts">
						<xs:annotation>
							<xs:documentation>parent ComposedShader node</xs:documentation>
						</xs:annotation>
					</xs:attribute>
					<xs:attribute name="url" type="MFString"/>
					<xs:attribute name="type" type="shaderPartTypeValues"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ShaderProgram">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DNode,X3DUrlObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/shaders.html#ShaderProgram"/>
		</xs:annotation>
		<xs:complexType mixed="true">
			<xs:complexContent>
				<xs:extension base="X3DProgrammableShaderObject">
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="programs"/>
					<xs:attribute name="url" type="MFString"/>
					<xs:attribute name="type" type="shaderPartTypeValues"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ComposedCubeMapTexture">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/env_texture.html#ComposedCubeMapTexture"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DEnvironmentTextureNode">
					<xs:sequence-cl minOccurs="0" maxOccurs="6">
						<xs:choice>
							<xs:element ref="ImageTexture"/>
							<xs:element ref="PixelTexture"/>
							<xs:element ref="MovieTexture"/>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:choice>
					</xs:sequence-cl>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="GeneratedCubeMapTexture">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/env_texture.html#GeneratedCubeMapTexture"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DEnvironmentTextureNode">
					<xs:attribute name="update" default="NONE">
						<xs:simpleType>
							<xs:restriction base="SFString">
								<xs:enumeration value="NONE"/>
								<xs:enumeration value="NEXT_FRAME_ONLY"/>
								<xs:enumeration value="ALWAYS"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="size" default="128">
						<xs:simpleType>
							<xs:restriction base="SFInt32">
								<xs:minExclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ImageCubeMapTexture">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DUrlObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/env_texture.html#ImageCubeMapTexture"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DEnvironmentTextureNode">
					<xs:attribute name="url" type="MFString"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ComposedTexture3D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texture3D.html#ComposedTexture3D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTexture3DNode">
					<xs:sequence-cl minOccurs="0" maxOccurs="unbounded">
						<xs:annotation>
							<xs:documentation>texture</xs:documentation>
						</xs:annotation>
						<xs:element ref="ImageTexture"/>
						<xs:element ref="PixelTexture"/>
						<xs:element ref="MovieTexture"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:sequence-cl>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ImageTexture3D">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DUrlObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texture3D.html#ImageTexture3D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTexture3DNode">
					<xs:attribute name="url" type="MFString"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="PixelTexture3D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texture3D.html#PixelTexture3D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTexture3DNode">
					<xs:attribute name="image" type="MFInt32" default="0 0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TextureCoordinate3D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texture3D.html#TextureCoordinate3D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTextureCoordinateNode">
					<xs:attribute name="point" type="MFVec3f"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TextureCoordinate4D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texture3D.html#TextureCoordinate4D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTextureCoordinateNode">
					<xs:attribute name="point" type="MFVec4f"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TextureTransformMatrix3D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texture3D.html#TextureTransformMatrix3D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTextureTransformNode">
					<xs:attribute name="matrix" type="SFMatrix4f" default="1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TextureTransform3D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texture3D.html#TextureTransform3D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DTextureTransformNode">
					<xs:attribute name="center" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="rotation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="scale" type="SFVec3f" default="1 1 1"/>
					<xs:attribute name="translation" type="SFVec3f" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:complexType name="X3DViewpointNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/navigation.html#X3DViewpointNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DBindableNode">
				<xs:attribute name="description" type="SFString"/>
				<xs:attribute name="jump" type="SFBool" default="true"/>
				<xs:attribute name="orientation" type="SFRotation" default="0 0 1 0"/>
				<xs:attribute name="position" type="SFVec3f" default="0 0 10"/>
				<xs:attribute name="retainUserOffsets" type="SFBool" default="false"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:element name="OrthoViewpoint">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/navigation.html#OrthoViewpoint"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DViewpointNode">
					<xs:attribute name="centerOfRotation" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="fieldOfView" type="MFFloat" default="-1 -1 1 1"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ViewpointGroup">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/navigation.html#ViewpointGroup"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNode">
					<xs:choice minOccurs="0" maxOccurs="unbounded">
						<xs:element ref="Viewpoint"/>
						<xs:element ref="OrthoViewpoint"/>
						<xs:element ref="ViewpointGroup"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="center" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="description" type="SFString"/>
					<xs:attribute name="displayed" type="SFBool" default="true"/>
					<xs:attribute name="retainUserOffsets" type="SFBool" default="false"/>
					<xs:attribute name="size" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="children"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:complexType name="X3DLayerNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="/Part01/components/layering.html#X3DLayerNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DNode">
				<xs:attribute name="isPickable" type="SFBool" default="true"/>
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="layers"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DViewportNode">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/layering.html#X3DViewportNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DGroupingNode">
				<xs:annotation>
					<xs:appinfo>
						<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DBoundedObject"/>
					</xs:appinfo>
					<xs:documentation/>
				</xs:annotation>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:element name="Layer">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/layering.html#Layer"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DLayerNode">
					<xs:choice>
						<xs:sequence-cl>
							<xs:element ref="Viewport">
								<xs:annotation>
									<xs:documentation>viewport</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:group ref="ChildContentModel" minOccurs="0"/>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:group ref="ChildContentModel"/>
							<xs:element ref="Viewport" minOccurs="0">
								<xs:annotation>
									<xs:documentation>viewport</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:group ref="ChildContentModel" minOccurs="0"/>
						</xs:sequence-cl>
					</xs:choice>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="LayerSet">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/layering.html#LayerSet"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNode">
					<xs:choice minOccurs="0" maxOccurs="unbounded">
						<xs:annotation>
							<xs:documentation>layers</xs:documentation>
						</xs:annotation>
						<xs:element ref="Layer"/>
						<xs:element ref="LayoutLayer"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="activeLayer" type="SFInt32" default="0"/>
					<xs:attribute name="order" type="MFInt32" default="0"/>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="children"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Viewport">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/layering.html#Viewport"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DViewportNode">
					<xs:attribute name="clipBoundary" type="MFFloat" default="0 1 0 1">
						<xs:annotation>
							<xs:documentation>TODO:  put bounds [0..1] on values if possible</xs:documentation>
						</xs:annotation>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:complexType name="X3DLayoutNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/layout.html#X3DLayoutNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DChildNode"/>
		</xs:complexContent>
	</xs:complexType>
	<xs:group name="LayoutGroupContentModel">
		<xs:annotation>
			<xs:appinfo>LayoutGroupContentModel can contain other nodes as children, plus an X3DLayoutNode as a layout field.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/layout.html#LayoutGroup"/>
		</xs:annotation>
		<xs:choice>
			<xs:sequence-cl>
				<xs:element ref="Layout">
					<xs:annotation>
						<xs:documentation>layout</xs:documentation>
					</xs:annotation>
				</xs:element>
				<xs:element ref="Viewport" minOccurs="0">
					<xs:annotation>
						<xs:documentation>viewport</xs:documentation>
					</xs:annotation>
				</xs:element>
				<xs:group ref="ChildContentModel" minOccurs="0" maxOccurs="unbounded"/>
			</xs:sequence-cl>
			<xs:sequence-cl>
				<xs:element ref="Viewport">
					<xs:annotation>
						<xs:documentation>viewport</xs:documentation>
					</xs:annotation>
				</xs:element>
				<xs:element ref="Layout" minOccurs="0">
					<xs:annotation>
						<xs:documentation>layout</xs:documentation>
					</xs:annotation>
				</xs:element>
				<xs:group ref="ChildContentModel" minOccurs="0" maxOccurs="unbounded"/>
			</xs:sequence-cl>
			<xs:group ref="ChildContentModel" minOccurs="0" maxOccurs="unbounded"/>
		</xs:choice>
	</xs:group>
	<xs:element name="Layout">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/layout.html#Layout"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DLayoutNode">
					<xs:attribute name="align" type="MFString" default='"CENTER" "CENTER"'/>
					<xs:attribute name="offset" type="MFFloat" default="0 0"/>
					<xs:attribute name="offsetUnits" type="MFString" default='"WORLD" "WORLD"'/>
					<xs:attribute name="scaleMode" type="MFString" default='"NONE" "NONE"'/>
					<xs:attribute name="size" type="MFFloat" default="1 1"/>
					<xs:attribute name="sizeUnits" type="MFString" default='"WORLD" "WORLD"'/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="LayoutGroup">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/layout.html#LayoutGroup"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNode">
					<xs:group ref="LayoutGroupContentModel"/>
					<xs:attribute name="containerField" type="xs:NMTOKEN" default="children"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="LayoutLayer">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/layout.html#LayoutLayer"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DLayerNode">
					<xs:group ref="LayoutGroupContentModel"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ScreenFontStyle">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/layout.html#ScreenFontStyle"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DFontStyleNode">
					<xs:attribute name="family" type="MFString" default='"SERIF"'/>
					<xs:attribute name="horizontal" type="SFBool" default="true"/>
					<xs:attribute name="justify" type="MFString" default='"BEGIN"'/>
					<xs:attribute name="language" type="SFString"/>
					<xs:attribute name="leftToRight" type="SFBool" default="true"/>
					<xs:attribute name="pointSize" type="SFFloat" default="12.0"/>
					<xs:attribute name="spacing" type="SFFloat" default="1.0"/>
					<xs:attribute name="style" type="fontStyleValues" default="PLAIN"/>
					<xs:attribute name="topToBottom" type="SFBool" default="true"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ScreenGroup">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/layout.html#ScreenGroup"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:complexType name="X3DNBodyCollidableNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#X3DNBodyCollidableNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DChildNode">
				<xs:annotation>
					<xs:appinfo>
						<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DBoundedObject"/>
					</xs:appinfo>
				</xs:annotation>
				<xs:attribute name="bboxCenter" type="SFVec3f" default="0 0 0"/>
				<xs:attribute name="bboxSize" type="boundingBoxSizeType" default="-1 -1 -1"/>
				<xs:attribute name="enabled" type="SFBool" default="true"/>
				<xs:attribute name="rotation" type="SFRotation" default="0 0 1 0"/>
				<xs:attribute name="translation" type="SFVec3f" default="0 0 0"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DNBodyCollisionSpaceNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#X3DNBodyCollisionSpaceNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DNode">
				<xs:annotation>
					<xs:appinfo>
						<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DBoundedObject"/>
					</xs:appinfo>
				</xs:annotation>
				<xs:attribute name="bboxCenter" type="SFVec3f" default="0 0 0"/>
				<xs:attribute name="bboxSize" type="boundingBoxSizeType" default="-1 -1 -1"/>
				<xs:attribute name="enabled" type="SFBool" default="true"/>
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="children"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DRigidJointNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#X3DRigidJointNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DNode">
				<xs:annotation>
					<xs:appinfo>
						<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DBoundedObject"/>
					</xs:appinfo>
				</xs:annotation>
                                <xs:group ref="RigidJointNodeContentModel" minOccurs="0"/>
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="joints"/>
				<xs:attribute name="mustOutput" type="MFString" default="NONE"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:group name="RigidJointNodeContentModel">
		<xs:annotation>
			<xs:appinfo>RigidJointNodeContentModel can contain two RigidBody nodes as body1, body2 fields.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#X3DRigidJointNode"/>
		</xs:annotation>
                <xs:choice>
                        <xs:sequence-cl>
                                <xs:element ref="RigidBody">
                                        <xs:annotation>
                                                <xs:documentation>body1, body2</xs:documentation>
                                        </xs:annotation>
                                </xs:element>
                                <xs:choice minOccurs="0">
                                        <xs:element ref="RigidBody">
                                                <xs:annotation>
                                                        <xs:documentation>body2, body1</xs:documentation>
                                                </xs:annotation>
                                        </xs:element>
                                        <xs:element ref="ProtoInstance">
                                                <xs:annotation>
                                                        <xs:documentation>Appropriately typed substitute node</xs:documentation>
                                                </xs:annotation>
                                        </xs:element>
                                </xs:choice>
                        </xs:sequence-cl>
                        <xs:sequence-cl>
                                <xs:element ref="ProtoInstance">
                                        <xs:annotation>
                                                <xs:documentation>body1, body2</xs:documentation>
                                        </xs:annotation>
                                </xs:element>
                                <xs:choice minOccurs="0">
                                        <xs:element ref="RigidBody">
                                                <xs:annotation>
                                                        <xs:documentation>body2, body1</xs:documentation>
                                                </xs:annotation>
                                        </xs:element>
                                        <xs:element ref="ProtoInstance">
                                                <xs:annotation>
                                                        <xs:documentation>Appropriately typed substitute node</xs:documentation>
                                                </xs:annotation>
                                        </xs:element>
                                </xs:choice>
                        </xs:sequence-cl>
                </xs:choice>
	</xs:group>
	<xs:group name="RigidBodyContentModel">
		<xs:annotation>
			<xs:appinfo>RigidBodyContentModel can contain X3DNBodyCollidableNode nodes as a geometry field, plus a [Sphere, Box, Cone] as a massDensityModel field.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#RigidBody"/>
		</xs:annotation>
		<xs:choice>
			<xs:sequence-cl>
				<xs:choice>
					<xs:element ref="Sphere">
						<xs:annotation>
							<xs:documentation>massDensityModel</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="Box">
						<xs:annotation>
							<xs:documentation>massDensityModel</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="Cone">
						<xs:annotation>
							<xs:documentation>massDensityModel</xs:documentation>
						</xs:annotation>
					</xs:element>
				</xs:choice>
				<xs:choice minOccurs="0" maxOccurs="unbounded">
					<xs:element ref="CollidableOffset">
						<xs:annotation>
							<xs:documentation>geometry</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="CollidableShape">
						<xs:annotation>
							<xs:documentation>geometry</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="ProtoInstance">
						<xs:annotation>
							<xs:documentation>Appropriately typed substitute node</xs:documentation>
						</xs:annotation>
					</xs:element>
				</xs:choice>
			</xs:sequence-cl>
			<xs:sequence-cl>
				<xs:choice maxOccurs="unbounded">
					<xs:element ref="CollidableOffset">
						<xs:annotation>
							<xs:documentation>geometry</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="CollidableShape">
						<xs:annotation>
							<xs:documentation>geometry</xs:documentation>
						</xs:annotation>
					</xs:element>
				</xs:choice>
				<xs:choice minOccurs="0">
					<xs:element ref="Sphere">
						<xs:annotation>
							<xs:documentation>massDensityModel</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="Box">
						<xs:annotation>
							<xs:documentation>massDensityModel</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="Cone">
						<xs:annotation>
							<xs:documentation>massDensityModel</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="ProtoInstance">
						<xs:annotation>
							<xs:documentation>Appropriately typed substitute node</xs:documentation>
						</xs:annotation>
					</xs:element>
				</xs:choice>
			</xs:sequence-cl>
			<xs:sequence-cl>
				<xs:element ref="ProtoInstance">
					<xs:annotation>
						<xs:documentation>Appropriately typed substitute node</xs:documentation>
					</xs:annotation>
				</xs:element>
				<xs:choice minOccurs="0">
					<xs:element ref="Sphere">
						<xs:annotation>
							<xs:documentation>massDensityModel</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="Box">
						<xs:annotation>
							<xs:documentation>massDensityModel</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="Cone">
						<xs:annotation>
							<xs:documentation>massDensityModel</xs:documentation>
						</xs:annotation>
					</xs:element>
				</xs:choice>
				<xs:choice minOccurs="0" maxOccurs="unbounded">
					<xs:element ref="CollidableOffset">
						<xs:annotation>
							<xs:documentation>geometry</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="CollidableShape">
						<xs:annotation>
							<xs:documentation>geometry</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="ProtoInstance">
						<xs:annotation>
							<xs:documentation>Appropriately typed substitute node</xs:documentation>
						</xs:annotation>
					</xs:element>
				</xs:choice>
			</xs:sequence-cl>
		</xs:choice>
	</xs:group>
	<xs:group name="RigidBodyCollectionContentModel">
		<xs:annotation>
			<xs:appinfo>RigidBodyCollectionContentModel can contain RigidBody nodes as a bodies field, plus X3DRigidJointNode nodes as a joints field, plus a CollisionCollection node as a collider field.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#RigidBodyCollection"/>
		</xs:annotation>
		<xs:choice>
			<xs:sequence-cl>
				<xs:element ref="CollisionCollection">
					<xs:annotation>
						<xs:documentation>collider</xs:documentation>
					</xs:annotation>
				</xs:element>
				<xs:choice>
					<xs:sequence-cl minOccurs="0">
						<xs:choice maxOccurs="unbounded">
							<xs:element ref="RigidBody">
								<xs:annotation>
									<xs:documentation>bodies</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:choice>
						<xs:choice minOccurs="0" maxOccurs="unbounded">
							<xs:annotation>
								<xs:documentation>joints</xs:documentation>
							</xs:annotation>
							<xs:element ref="BallJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="DoubleAxisHingeJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="MotorJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="SingleAxisHingeJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="SliderJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="UniversalJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:choice>
					</xs:sequence-cl>
					<xs:sequence-cl minOccurs="0">
						<xs:choice maxOccurs="unbounded">
							<xs:annotation>
								<xs:documentation>joints</xs:documentation>
							</xs:annotation>
							<xs:element ref="BallJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="DoubleAxisHingeJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="MotorJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="SingleAxisHingeJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="SliderJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="UniversalJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:choice>
						<xs:choice minOccurs="0" maxOccurs="unbounded">
							<xs:element ref="RigidBody">
								<xs:annotation>
									<xs:documentation>bodies</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:choice>
					</xs:sequence-cl>
				</xs:choice>
			</xs:sequence-cl>
			<xs:sequence-cl>
				<xs:choice maxOccurs="unbounded">
					<xs:element ref="RigidBody">
						<xs:annotation>
							<xs:documentation>bodies</xs:documentation>
						</xs:annotation>
					</xs:element>
				</xs:choice>
				<xs:choice>
					<xs:sequence-cl minOccurs="0">
						<xs:element ref="CollisionCollection">
							<xs:annotation>
								<xs:documentation>collider</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:choice minOccurs="0" maxOccurs="unbounded">
							<xs:annotation>
								<xs:documentation>joints</xs:documentation>
							</xs:annotation>
							<xs:element ref="BallJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="DoubleAxisHingeJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="MotorJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="SingleAxisHingeJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="SliderJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="UniversalJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:choice>
					</xs:sequence-cl>
					<xs:sequence-cl minOccurs="0">
						<xs:choice maxOccurs="unbounded">
							<xs:annotation>
								<xs:documentation>joints</xs:documentation>
							</xs:annotation>
							<xs:element ref="BallJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="DoubleAxisHingeJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="MotorJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="SingleAxisHingeJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="SliderJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="UniversalJoint">
								<xs:annotation>
									<xs:documentation>massDensityModel</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:choice>
						<xs:element ref="CollisionCollection" minOccurs="0">
							<xs:annotation>
								<xs:documentation>collider</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:choice minOccurs="0" maxOccurs="unbounded">
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:choice>
					</xs:sequence-cl>
				</xs:choice>
			</xs:sequence-cl>
			<xs:sequence-cl>
				<xs:choice maxOccurs="unbounded">
					<xs:annotation>
						<xs:documentation>joints</xs:documentation>
					</xs:annotation>
					<xs:element ref="BallJoint">
						<xs:annotation>
							<xs:documentation>massDensityModel</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="DoubleAxisHingeJoint">
						<xs:annotation>
							<xs:documentation>massDensityModel</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="MotorJoint">
						<xs:annotation>
							<xs:documentation>massDensityModel</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="SingleAxisHingeJoint">
						<xs:annotation>
							<xs:documentation>massDensityModel</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="SliderJoint">
						<xs:annotation>
							<xs:documentation>massDensityModel</xs:documentation>
						</xs:annotation>
					</xs:element>
					<xs:element ref="UniversalJoint">
						<xs:annotation>
							<xs:documentation>massDensityModel</xs:documentation>
						</xs:annotation>
					</xs:element>
				</xs:choice>
				<xs:choice>
					<xs:sequence-cl minOccurs="0">
						<xs:element ref="CollisionCollection">
							<xs:annotation>
								<xs:documentation>collider</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:choice minOccurs="0" maxOccurs="unbounded">
							<xs:element ref="RigidBody">
								<xs:annotation>
									<xs:documentation>bodies</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:choice>
					</xs:sequence-cl>
					<xs:sequence-cl minOccurs="0">
						<xs:choice maxOccurs="unbounded">
							<xs:element ref="RigidBody">
								<xs:annotation>
									<xs:documentation>bodies</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:choice>
						<xs:element ref="CollisionCollection" minOccurs="0">
							<xs:annotation>
								<xs:documentation>collider</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:choice minOccurs="0" maxOccurs="unbounded">
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:choice>
					</xs:sequence-cl>
				</xs:choice>
			</xs:sequence-cl>
		</xs:choice>
	</xs:group>
	<xs:element name="BallJoint">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#BallJoint"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DRigidJointNode">
					<xs:attribute name="anchorPoint" type="SFVec3f" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="CollidableOffset">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#CollidableOffset"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNBodyCollidableNode">
					<xs:choice minOccurs="0">
						<xs:element ref="CollidableOffset">
							<xs:annotation>
								<xs:documentation>collidable</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="CollidableShape">
							<xs:annotation>
								<xs:documentation>collidable</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="CollidableShape">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#CollidableShape"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNBodyCollidableNode">
					<xs:choice minOccurs="0">
						<xs:element ref="Shape">
							<xs:annotation>
								<xs:documentation>shape</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="CollisionCollection">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#CollisionCollection"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChildNode">
					<xs:choice minOccurs="0" maxOccurs="unbounded">
						<xs:annotation>
							<xs:documentation>collidables</xs:documentation>
						</xs:annotation>
						<xs:element ref="CollidableOffset"/>
						<xs:element ref="CollidableShape"/>
						<xs:element ref="CollisionSpace"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="appliedParameters" type="MFString" default="BOUNCE"/>
					<xs:attribute name="bounce" type="SFFloat" default="0">
						<xs:annotation>
							<xs:documentation>TODO:  range  [0,1]</xs:documentation>
						</xs:annotation>
					</xs:attribute>
					<xs:attribute name="enabled" type="SFBool" default="true"/>
					<xs:attribute name="frictionCoefficients" type="SFVec2f" default="0 0"/>
					<xs:attribute name="minBounceSpeed" type="SFFloat" default="0.1"/>
					<xs:attribute name="slipFactors" type="SFVec2f" default="0 0"/>
					<xs:attribute name="softnessConstantForceMix" type="SFFloat" default="0.0001"/>
					<xs:attribute name="softnessErrorCorrection" type="SFFloat" default="0.8"/>
					<xs:attribute name="surfaceSpeed" type="SFVec2f" default="0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="CollisionSensor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#CollisionSensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChildNode">
					<xs:choice minOccurs="0">
						<xs:annotation>
							<xs:documentation>collider</xs:documentation>
						</xs:annotation>
						<xs:element ref="CollisionCollection"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="enabled" type="SFBool" default="true"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="CollisionSpace">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#CollisionSpace"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNBodyCollisionSpaceNode">
					<xs:choice minOccurs="0" maxOccurs="unbounded">
						<xs:element ref="CollidableOffset">
							<xs:annotation>
								<xs:documentation>collider</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="CollidableShape">
							<xs:annotation>
								<xs:documentation>collider</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="CollisionSpace">
							<xs:annotation>
								<xs:documentation>collider</xs:documentation>
							</xs:annotation>
						</xs:element>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="useGeometry" type="SFBool" default="false"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="Contact">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#Contact"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNBodyCollisionSpaceNode">
					<xs:sequence-cl>
						<xs:sequence-cl minOccurs="0">
							<xs:element ref="RigidBody">
								<xs:annotation>
									<xs:documentation>body1</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="RigidBody" minOccurs="0">
								<xs:annotation>
									<xs:documentation>body2</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:sequence-cl>
						<xs:choice minOccurs="0">
							<xs:sequence-cl>
								<xs:annotation>
									<xs:documentation>geometry1</xs:documentation>
								</xs:annotation>
								<xs:element ref="CollidableOffset"/>
								<xs:choice minOccurs="0">
									<xs:annotation>
										<xs:documentation>geometry2</xs:documentation>
									</xs:annotation>
									<xs:element ref="CollidableOffset"/>
									<xs:element ref="CollidableShape"/>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
								</xs:choice>
							</xs:sequence-cl>
							<xs:sequence-cl>
								<xs:annotation>
									<xs:documentation>geometry1</xs:documentation>
								</xs:annotation>
								<xs:element ref="CollidableShape"/>
								<xs:choice minOccurs="0">
									<xs:annotation>
										<xs:documentation>geometry2</xs:documentation>
									</xs:annotation>
									<xs:element ref="CollidableOffset"/>
									<xs:element ref="CollidableShape"/>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
								</xs:choice>
							</xs:sequence-cl>
							<xs:sequence-cl>
								<xs:annotation>
									<xs:documentation>geometry1</xs:documentation>
								</xs:annotation>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
								<xs:choice minOccurs="0">
									<xs:annotation>
										<xs:documentation>geometry2</xs:documentation>
									</xs:annotation>
									<xs:element ref="CollidableOffset"/>
									<xs:element ref="CollidableShape"/>
									<xs:element ref="ProtoInstance">
										<xs:annotation>
											<xs:documentation>Appropriately typed substitute node</xs:documentation>
										</xs:annotation>
									</xs:element>
								</xs:choice>
							</xs:sequence-cl>
						</xs:choice>
					</xs:sequence-cl>
					<xs:attribute name="appliedParameters" type="MFString" default="BOUNCE"/>
					<xs:attribute name="bounce" type="SFFloat" default="0"/>
					<xs:attribute name="contactNormal" type="SFVec3f" default="0 1 0"/>
					<xs:attribute name="depth" type="SFFloat" default="0"/>
					<xs:attribute name="frictionCoefficients" type="SFVec2f" default="0 0"/>
					<xs:attribute name="frictionDirection" type="SFVec3f" default="0 1 0"/>
					<xs:attribute name="minBounceSpeed" type="SFFloat" default="0"/>
					<xs:attribute name="position" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="slipCoefficients" type="SFVec2f" default="0 0"/>
					<xs:attribute name="softnessConstantForceMix" type="SFFloat" default="0.0001"/>
					<xs:attribute name="softnessErrorCorrection" type="SFFloat" default="0.8"/>
					<xs:attribute name="surfaceSpeed" type="SFVec2f" default="0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="DoubleAxisHingeJoint">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#DoubleAxisHingeJoint"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DRigidJointNode">
					<xs:attribute name="anchorPoint" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="axis1" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="axis2" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="desiredAngularVelocity1" type="SFFloat" default="0"/>
					<xs:attribute name="desiredAngularVelocity2" type="SFFloat" default="0"/>
					<xs:attribute name="maxAngle1" type="SFFloat" default="3.141592653"/>
					<xs:attribute name="maxTorque1" type="SFFloat" default="0"/>
					<xs:attribute name="maxTorque2" type="SFFloat" default="0"/>
					<xs:attribute name="minAngle1" type="SFFloat" default="-3.141592653"/>
					<xs:attribute name="stop1Bounce" type="SFFloat" default="0"/>
					<xs:attribute name="stop1ConstantForceMix" type="SFFloat" default="0.001"/>
					<xs:attribute name="stop1ErrorCorrection" type="SFFloat" default="0.8"/>
					<xs:attribute name="suspensionErrorCorrection" type="SFFloat" default="0.8"/>
					<xs:attribute name="suspensionForce" type="SFFloat" default="0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="MotorJoint">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#MotorJoint"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DRigidJointNode">
					<xs:attribute name="autoCalc" type="SFBool" default="false"/>
					<xs:attribute name="axis1Angle" type="SFFloat" default="0"/>
					<xs:attribute name="axis1Torque" type="SFFloat" default="0"/>
					<xs:attribute name="axis2Angle" type="SFFloat" default="0"/>
					<xs:attribute name="axis2Torque" type="SFFloat" default="0"/>
					<xs:attribute name="axis3Angle" type="SFFloat" default="0"/>
					<xs:attribute name="axis3Torque" type="SFFloat" default="0"/>
					<xs:attribute name="enabledAxes" default="1">
						<xs:simpleType>
							<xs:restriction base="SFInt32">
								<xs:minInclusive value="0"/>
								<xs:maxInclusive value="3"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="motor1Axis" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="motor2Axis" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="motor3Axis" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="stop1Bounce" type="SFFloat" default="0"/>
					<xs:attribute name="stop1ErrorCorrection" type="SFFloat" default="0.8"/>
					<xs:attribute name="stop2Bounce" type="SFFloat" default="0"/>
					<xs:attribute name="stop2ErrorCorrection" type="SFFloat" default="0.8"/>
					<xs:attribute name="stop3Bounce" type="SFFloat" default="0"/>
					<xs:attribute name="stop3ErrorCorrection" type="SFFloat" default="0.8"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="RigidBody">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#RigidBody"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DNode">
					<xs:group ref="RigidBodyContentModel" minOccurs="0"/>
					<xs:attribute name="angularDampingFactor" type="SFFloat" default="0.001"/>
					<xs:attribute name="angularVelocity" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="autoDamp" type="SFBool" default="false"/>
					<xs:attribute name="autoDisable" type="SFBool" default="false"/>
					<xs:attribute name="centerOfMass" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="disableAngularSpeed" type="SFFloat" default="0"/>
					<xs:attribute name="disableLinearSpeed" type="SFFloat" default="0"/>
					<xs:attribute name="disableTime" type="SFTime" default="0"/>
					<xs:attribute name="enabled" type="SFBool" default="true"/>
					<xs:attribute name="finiteRotationAxis" type="SFVec3f" default="0 1 0"/>
					<xs:attribute name="fixed" type="SFBool" default="false"/>
					<xs:attribute name="forces" type="MFVec3f"/>
					<xs:attribute name="inertia" type="SFMatrix3f" default="1 0 0 0 1 0 0 0 1"/>
					<xs:attribute name="linearDampingFactor" type="SFFloat" default="0.001"/>
					<xs:attribute name="linearVelocity" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="mass" type="SFFloat" default="1"/>
					<xs:attribute name="position" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="orientation" type="SFRotation" default="0 0 1 0"/>
					<xs:attribute name="torques" type="MFVec3f"/>
					<xs:attribute name="useFiniteRotation" type="SFBool" default="false"/>
					<xs:attribute name="useGlobalGravity" type="SFBool" default="true"/>
					<xs:attribute name="containerField" default="bodies">
						<xs:simpleType>
							<xs:restriction base="xs:NMTOKEN">
								<xs:enumeration value="body1"/>
								<xs:enumeration value="body2"/>
								<xs:enumeration value="bodies"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="RigidBodyCollection">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#RigidBodyCollection"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChildNode">
					<xs:group ref="RigidBodyCollectionContentModel" minOccurs="0"/>
					<xs:attribute name="autoDisable" type="SFBool" default="false"/>
					<xs:attribute name="constantForceMix" type="SFFloat" default="0.0001"/>
					<xs:attribute name="contactSurfaceThickness" type="SFFloat" default="0"/>
					<xs:attribute name="disableAngularSpeed" type="SFFloat" default="0"/>
					<xs:attribute name="disableLinearSpeed" type="SFFloat" default="0"/>
					<xs:attribute name="disableTime" type="SFTime" default="0"/>
					<xs:attribute name="enabled" type="SFBool" default="true"/>
					<xs:attribute name="errorCorrection" type="SFFloat" default="0.8"/>
					<xs:attribute name="gravity" type="SFVec3f" default="0 -9.8 0"/>
					<xs:attribute name="iterations" type="SFInt32" default="10"/>
					<xs:attribute name="maxCorrectionSpeed" type="SFFloat" default="-1"/>
					<xs:attribute name="preferAccuracy" type="SFBool" default="false"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="SingleAxisHingeJoint">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#SingleAxisHingeJoint"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DRigidJointNode">
					<xs:attribute name="anchorPoint" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="axis" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="maxAngle" type="SFFloat" default="3.141592653"/>
					<xs:attribute name="minAngle" type="SFFloat" default="-3.141592653"/>
					<xs:attribute name="stopBounce" type="SFFloat" default="0"/>
					<xs:attribute name="stopErrorCorrection" type="SFFloat" default="0.8"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="SliderJoint">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#SliderJoint"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DRigidJointNode">
					<xs:attribute name="axis" type="SFVec3f" default="0 1 0"/>
					<xs:attribute name="maxSeparation" type="SFFloat" default="1"/>
					<xs:attribute name="minSeparation" type="SFFloat" default="0"/>
					<xs:attribute name="sliderForce" type="SFFloat" default="0"/>
					<xs:attribute name="stopBounce" type="SFFloat" default="0"/>
					<xs:attribute name="stopErrorCorrection" type="SFFloat" default="1"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="UniversalJoint">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/rigid_physics.html#UniversalJoint"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DRigidJointNode">
					<xs:attribute name="anchorPoint" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="axis1" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="axis2" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="stop1Bounce" type="SFFloat" default="0"/>
					<xs:attribute name="stop1ErrorCorrection" type="SFFloat" default="0.8"/>
					<xs:attribute name="stop2Bounce" type="SFFloat" default="0"/>
					<xs:attribute name="stop2ErrorCorrection" type="SFFloat" default="0.8"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:group name="PickSensorGroupingNodeContentModel">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to the X3DPickSensorNode type.  PickSensorGroupingNodeContentModel can contain a single X3DGeometryNode as pickingGeometry, plus an array of X3DGroupingNode, X3DShapeNode and Inline nodes as pickTarget.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/picking.html#X3DPickSensorNode"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="Anchor"/>
			<xs:element ref="Billboard"/>
			<xs:element ref="Collision"/>
			<xs:element ref="Group"/>
			<xs:element ref="LOD"/>
			<xs:element ref="StaticGroup"/>
			<xs:element ref="Switch"/>
			<xs:element ref="Transform"/>
			<xs:element ref="EspduTransform"/>
			<xs:element ref="ReceiverPdu"/>
			<xs:element ref="SignalPdu"/>
			<xs:element ref="TransmitterPdu"/>
			<xs:element ref="GeoLocation"/>
			<xs:element ref="GeoLOD"/>
			<xs:element ref="GeoTransform"/>
			<xs:element ref="HAnimJoint"/>
			<xs:element ref="NurbsSet"/>
			<xs:element ref="CADAssembly"/>
			<xs:element ref="CADLayer"/>
			<xs:element ref="CADPart"/>
			<xs:element ref="Viewport"/>
			<xs:element ref="LayoutGroup"/>
			<xs:element ref="ScreenGroup"/>
		</xs:choice>
	</xs:group>
	<xs:complexType name="X3DPickableObject" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/picking.html#X3DPickableObject"/>
		</xs:annotation>
		<xs:attribute name="objectType" type="MFString" default="ALL"/>
		<xs:attribute name="pickable" type="SFBool" default="true"/>
	</xs:complexType>
	<xs:complexType name="X3DPickSensorNode" abstract="true" mixed="false">
		<xs:complexContent>
			<xs:extension base="X3DSensorNode">
				<xs:choice>
					<xs:sequence-cl>
						<xs:group ref="GeometryContentModel">
							<xs:annotation>
								<xs:documentation>pickingGeometry</xs:documentation>
							</xs:annotation>
						</xs:group>
						<xs:choice minOccurs="0" maxOccurs="unbounded">
							<xs:annotation>
								<xs:documentation>pickTarget</xs:documentation>
							</xs:annotation>
							<xs:group ref="PickSensorGroupingNodeContentModel">
								<xs:annotation>
									<xs:documentation>pickingGeometry</xs:documentation>
								</xs:annotation>
							</xs:group>
							<xs:element ref="Shape"/>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:choice>
					</xs:sequence-cl>
					<xs:sequence-cl>
						<xs:choice maxOccurs="unbounded">
							<xs:annotation>
								<xs:documentation>pickTarget</xs:documentation>
							</xs:annotation>
							<xs:group ref="PickSensorGroupingNodeContentModel">
								<xs:annotation>
									<xs:documentation>pickingGeometry</xs:documentation>
								</xs:annotation>
							</xs:group>
							<xs:element ref="Shape"/>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>Appropriately typed substitute node</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:choice>
						<xs:group ref="GeometryContentModel" minOccurs="0">
							<xs:annotation>
								<xs:documentation>pickingGeometry</xs:documentation>
							</xs:annotation>
						</xs:group>
					</xs:sequence-cl>
				</xs:choice>
				<xs:attribute name="objectType" type="MFString" default="ALL"/>
				<xs:attribute name="intersectionType" type="SFString" default="BOUNDS"/>
				<xs:attribute name="sortOrder" type="SFString" default="CLOSEST"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:element name="LinePickSensor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/picking.html#LinePickSensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DPickSensorNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="PickableGroup">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DPickableObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/picking.html#PickableGroup"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DGroupingNode">
					<xs:sequence-cl minOccurs="0" maxOccurs="unbounded">
						<xs:element ref="NurbsPatchSurface"/>
						<xs:element ref="NurbsSweptSurface"/>
						<xs:element ref="NurbsSwungSurface"/>
						<xs:element ref="NurbsTrimmedSurface"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:sequence-cl>
					<xs:attribute name="objectType" type="MFString" default="ALL"/>
					<xs:attribute name="pickable" type="SFBool" default="true"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="PointPickSensor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/picking.html#PointPickSensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DPickSensorNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="PrimitivePickSensor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/picking.html#PrimitivePickSensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DPickSensorNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="VolumePickSensor">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/picking.html#VolumePickSensor"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DPickSensorNode"/>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:complexType name="X3DChaserNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html#X3DChaserNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DFollowerNode">
				<xs:attribute name="duration" type="SFTime" default="1"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DDamperNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html#X3DDamperNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DFollowerNode">
				<xs:attribute name="tau" type="SFTime" default="0.3"/>
				<xs:attribute name="tolerance" type="SFFloat" default="-1"/>
				<xs:attribute name="order" default="3">
					<xs:simpleType>
						<xs:restriction base="SFInt32">
							<xs:minInclusive value="0"/>
							<xs:maxInclusive value="5"/>
						</xs:restriction>
					</xs:simpleType>
				</xs:attribute>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DFollowerNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html#X3DFollowerNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DChildNode"/>
		</xs:complexContent>
	</xs:complexType>
	<xs:element name="ColorDamper">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html#ColorDamper"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DDamperNode">
					<xs:attribute name="initialDestination" type="SFColor" default="0.8 0.8 0.8"/>
					<xs:attribute name="initialValue" type="SFColor" default="0.8 0.8 0.8"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="CoordinateDamper">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html#CoordinateDamper"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DDamperNode">
					<xs:attribute name="initialDestination" type="MFVec3f" default="0 0 0"/>
					<xs:attribute name="initialValue" type="MFVec3f" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="OrientationDamper">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html#OrientationDamper"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DDamperNode">
					<xs:attribute name="initialDestination" type="SFRotation" default="0 1 0 0"/>
					<xs:attribute name="initialValue" type="SFRotation" default="0 1 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="PositionDamper">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html#PositionDamper"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DDamperNode">
					<xs:attribute name="initialDestination" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="initialValue" type="SFVec3f" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="PositionDamper2D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html#PositionDamper2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DDamperNode">
					<xs:attribute name="initialDestination" type="SFVec2f" default="0 0"/>
					<xs:attribute name="initialValue" type="SFVec2f" default="0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ScalarDamper">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html#ScalarDamper"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DDamperNode">
					<xs:attribute name="initialDestination" type="SFFloat" default="0"/>
					<xs:attribute name="initialValue" type="SFFloat" default="0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TexCoordDamper2D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html#TexCoordDamper2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DDamperNode">
					<xs:attribute name="initialDestination" type="MFVec2f"/>
					<xs:attribute name="initialValue" type="MFVec2f"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ColorChaser">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html#ColorChaser"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChaserNode">
					<xs:attribute name="initialDestination" type="SFColor" default="0.8 0.8 0.8"/>
					<xs:attribute name="initialValue" type="SFColor" default="0.8 0.8 0.8"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="CoordinateChaser">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html#CoordinateChaser"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChaserNode">
					<xs:attribute name="initialDestination" type="MFVec3f" default="0 0 0"/>
					<xs:attribute name="initialValue" type="MFVec3f" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="OrientationChaser">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html#OrientationChaser"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChaserNode">
					<xs:attribute name="initialDestination" type="SFRotation" default="0 1 0 0"/>
					<xs:attribute name="initialValue" type="SFRotation" default="0 1 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="PositionChaser">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html#PositionChaser"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChaserNode">
					<xs:attribute name="initialDestination" type="SFVec3f" default="0 0 0"/>
					<xs:attribute name="initialValue" type="SFVec3f" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="PositionChaser2D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html#PositionChaser2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChaserNode">
					<xs:attribute name="initialDestination" type="SFVec2f" default="0 0"/>
					<xs:attribute name="initialValue" type="SFVec2f" default="0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ScalarChaser">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html#ScalarChaser"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChaserNode">
					<xs:attribute name="initialDestination" type="SFFloat" default="0"/>
					<xs:attribute name="initialValue" type="SFFloat" default="0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="TexCoordChaser2D">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/followers.html#TexCoordChaser2D"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DChaserNode">
					<xs:attribute name="initialDestination" type="MFVec2f"/>
					<xs:attribute name="initialValue" type="MFVec2f"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:complexType name="X3DParticleEmitterNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/particle_systems.html#X3DParticleEmitterNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DNode">
				<xs:attribute name="speed" type="SFFloat" default="0"/>
				<xs:attribute name="variation" type="SFFloat" default="0.25"/>
				<xs:attribute name="mass" type="SFFloat" default="0"/>
				<xs:attribute name="surfaceArea" type="SFFloat" default="0"/>
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="emitter"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DParticlePhysicsModelNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/particle_systems.html#X3DParticlePhysicsModelNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DNode">
				<xs:attribute name="enabled" type="SFBool" default="true"/>
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="physics"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:element name="BoundedPhysicsModel">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/particle_systems.html#BoundedPhysicsModel"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DParticlePhysicsModelNode">
					<xs:sequence-cl minOccurs="0">
						<xs:annotation>
							<xs:documentation>geometry</xs:documentation>
						</xs:annotation>
						<xs:group ref="GeometryContentModel"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:sequence-cl>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ForcePhysicsModel">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/particle_systems.html#ForcePhysicsModel"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DParticlePhysicsModelNode">
					<xs:attribute name="force" type="SFVec3f" default="0 -9.8 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="WindPhysicsModel">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/particle_systems.html#WindPhysicsModel"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DParticlePhysicsModelNode">
					<xs:attribute name="direction" type="SFVec3f" default="1 0 0"/>
					<xs:attribute name="gustiness" type="SFFloat" default="0.1"/>
					<xs:attribute name="speed" type="SFFloat" default="0.1"/>
					<xs:attribute name="turbulence" type="SFFloat" default="0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ConeEmitter">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/particle_systems.html#ConeEmitter"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DParticleEmitterNode">
					<xs:attribute name="angle" type="SFFloat" default="0.7854"/>
					<xs:attribute name="direction" type="SFVec3f" default="0 1 0"/>
					<xs:attribute name="position" type="SFVec3f" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ExplosionEmitter">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/particle_systems.html#ExplosionEmitter"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DParticleEmitterNode">
					<xs:attribute name="position" type="SFVec3f" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="PointEmitter">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/particle_systems.html#PointEmitter"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DParticleEmitterNode">
					<xs:attribute name="direction" type="SFVec3f" default="0 1 0"/>
					<xs:attribute name="position" type="SFVec3f" default="0 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="PolylineEmitter">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/particle_systems.html#PolylineEmitter"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DParticleEmitterNode">
					<xs:choice minOccurs="0">
						<xs:annotation>
							<xs:documentation>coords</xs:documentation>
						</xs:annotation>
						<xs:element ref="Coordinate"/>
						<xs:element ref="CoordinateDouble"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="coordIndex" type="MFInt32" default="-1"/>
					<xs:attribute name="direction" type="SFVec3f" default="0 1 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="SurfaceEmitter">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/particle_systems.html#SurfaceEmitter"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DParticleEmitterNode">
					<xs:choice minOccurs="0">
						<xs:annotation>
							<xs:documentation>surface</xs:documentation>
						</xs:annotation>
						<xs:group ref="GeometryContentModel">
							<xs:annotation>
								<xs:documentation>surface</xs:documentation>
							</xs:annotation>
						</xs:group>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="VolumeEmitter">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/particle_systems.html#VolumeEmitter"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DParticleEmitterNode">
					<xs:choice minOccurs="0">
						<xs:annotation>
							<xs:documentation>coord</xs:documentation>
						</xs:annotation>
						<xs:element ref="Coordinate"/>
						<xs:element ref="CoordinateDouble"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="coordIndex" type="MFInt32" default="-1"/>
					<xs:attribute name="direction" type="SFVec3f" default="0 1 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ParticleSystem">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/particle_systems.html#ParticleSystem"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DShapeNode">
					<xs:sequence-cl>
						<xs:choice minOccurs="0">
							<xs:annotation>
								<xs:documentation>SFNode</xs:documentation>
							</xs:annotation>
							<xs:element ref="Color">
								<xs:annotation>
									<xs:documentation>colorRamp</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="ColorRGBA">
								<xs:annotation>
									<xs:documentation>colorRamp</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:choice>
						<xs:choice minOccurs="0">
							<xs:annotation>
								<xs:documentation>SFNode</xs:documentation>
							</xs:annotation>
							<xs:element ref="ConeEmitter">
								<xs:annotation>
									<xs:documentation>emitter</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="ExplosionEmitter">
								<xs:annotation>
									<xs:documentation>emitter</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="PointEmitter">
								<xs:annotation>
									<xs:documentation>emitter</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="PolylineEmitter">
								<xs:annotation>
									<xs:documentation>emitter</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="SurfaceEmitter">
								<xs:annotation>
									<xs:documentation>emitter</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="VolumeEmitter">
								<xs:annotation>
									<xs:documentation>emitter</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:choice>
						<xs:sequence-cl minOccurs="0" maxOccurs="unbounded">
							<xs:element ref="BoundedPhysicsModel">
								<xs:annotation>
									<xs:documentation>physics</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="ForcePhysicsModel">
								<xs:annotation>
									<xs:documentation>physics</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="WindPhysicsModel">
								<xs:annotation>
									<xs:documentation>physics</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>physics</xs:documentation>
								</xs:annotation>
							</xs:element>
						</xs:sequence-cl>
						<xs:element ref="TextureCoordinate" minOccurs="0">
							<xs:annotation>
								<xs:documentation>texCoordRamp</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:sequence-cl>
					<xs:attribute name="createParticles" type="SFBool" default="true"/>
					<xs:attribute name="enabled" type="SFBool" default="true"/>
					<xs:attribute name="lifetimeVariation" type="SFFloat" default="0.25"/>
					<xs:attribute name="maxParticles" type="SFInt32" default="200"/>
					<xs:attribute name="particleLifetime" type="SFFloat" default="5"/>
					<xs:attribute name="particleSize" type="SFVec2f" default="0.02 0.02"/>
					<xs:attribute name="colorKey" type="MFFloat"/>
					<xs:attribute name="geometryType" type="SFString" default="QUAD"/>
					<xs:attribute name="texCoordKey" type="MFFloat"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:complexType name="X3DComposableVolumeRenderStyleNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html#X3DComposableVolumeRenderStyleNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DVolumeRenderStyleNode"/>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DVolumeDataNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:appinfo>
				<xs:attribute name="otherInterfaces" type="xs:string" fixed="X3DBoundedObject"/>
			</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html#X3DVolumeDataNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DChildNode">
				<xs:attribute name="dimensions" type="SFVec3f" default="1 1 1"/>
				<xs:attribute name="bboxCenter" type="SFVec3f" default="0 0 0"/>
				<xs:attribute name="bboxSize" type="boundingBoxSizeType" default="-1 -1 -1"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:complexType name="X3DVolumeRenderStyleNode" abstract="true" mixed="false">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html#X3DVolumeRenderStyleNode"/>
		</xs:annotation>
		<xs:complexContent>
			<xs:extension base="X3DNode">
				<xs:attribute name="enabled" type="SFBool" default="true"/>
				<xs:attribute name="containerField" type="xs:NMTOKEN" default="renderStyle"/>
			</xs:extension>
		</xs:complexContent>
	</xs:complexType>
	<xs:group name="TextureContentModel">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DTextureNode nodes in Texturing and Texturing3D components.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#X3DTextureNode"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="ImageTexture"/>
			<xs:element ref="PixelTexture"/>
			<xs:element ref="MovieTexture"/>
			<xs:element ref="MultiTexture"/>
			<xs:element ref="ComposedTexture3D"/>
			<xs:element ref="ImageTexture3D"/>
			<xs:element ref="PixelTexture3D"/>
		</xs:choice>
	</xs:group>
	<xs:group name="Texture2DContentModel">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DTexture2DNode nodes in Texturing component.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#X3DTexture2DNode"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="ImageTexture"/>
			<xs:element ref="PixelTexture"/>
			<xs:element ref="MovieTexture"/>
		</xs:choice>
	</xs:group>
	<xs:group name="Texture3DContentModel">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DTexture3DNode nodes in Texturing3D component.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texture3D.html#X3DTexture3DNode"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="ComposedTexture3D"/>
			<xs:element ref="ImageTexture3D"/>
			<xs:element ref="PixelTexture3D"/>
		</xs:choice>
	</xs:group>
	<xs:group name="VolumeRenderStyleContentModel">
		<xs:annotation>
			<xs:appinfo>Child-node content model corresponding to X3DVolumeRenderStyleNode nodes in Volume Rendering component.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html"/>
		</xs:annotation>
		<xs:choice>
			<xs:element ref="BlendedVolumeStyle"/>
			<xs:element ref="BoundaryEnhancementVolumeStyle"/>
			<xs:element ref="CartoonVolumeStyle"/>
			<xs:element ref="ComposedVolumeStyle"/>
			<xs:element ref="EdgeEnhancementVolumeStyle"/>
			<xs:element ref="OpacityMapVolumeStyle"/>
			<xs:element ref="ProjectionVolumeStyle"/>
			<xs:element ref="ShadedVolumeStyle"/>
			<xs:element ref="SilhouetteEnhancementVolumeStyle"/>
			<xs:element ref="ToneMappedVolumeStyle"/>
		</xs:choice>
	</xs:group>
	<xs:simpleType name="volumeRenderingWeightFunctionTypes">
		<xs:annotation>
			<xs:appinfo>volumeRenderingWeightFunctionTypes are allowed enumeration values for BlendedVolumeStyle weightFunction* fields.</xs:appinfo>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html#t-WeightFunctionTypes"/>
		</xs:annotation>
		<xs:restriction base="xs:token">
			<xs:enumeration value="CONSTANT"/>
			<xs:enumeration value="ALPHA1"/>
			<xs:enumeration value="ALPHA2"/>
			<xs:enumeration value="ONE_MINUS_ALPHA1"/>
			<xs:enumeration value="ONE_MINUS_ALPHA2"/>
			<xs:enumeration value="TABLE"/>
		</xs:restriction>
	</xs:simpleType>
	<xs:element name="BlendedVolumeStyle">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html#BlendedVolumeStyle"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposableVolumeRenderStyleNode">
					<xs:choice minOccurs="0" maxOccurs="4">
						<xs:group ref="Texture2DContentModel">
							<xs:annotation>
								<xs:documentation>SFNode weightTransferFunction1 or SFNode weightTransferFunction2</xs:documentation>
							</xs:annotation>
						</xs:group>
						<xs:group ref="VolumeRenderStyleContentModel">
							<xs:annotation>
								<xs:documentation>SFNode renderStyle</xs:documentation>
							</xs:annotation>
						</xs:group>
						<xs:group ref="Texture3DContentModel">
							<xs:annotation>
								<xs:documentation>SFNode voxels</xs:documentation>
							</xs:annotation>
						</xs:group>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="weightConstant1" default="0.5">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
								<xs:maxInclusive value="1"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="weightConstant2" default="0.5">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
								<xs:maxInclusive value="1"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="weightFunction1" type="volumeRenderingWeightFunctionTypes" default="CONSTANT"/>
					<xs:attribute name="weightFunction2" type="volumeRenderingWeightFunctionTypes" default="CONSTANT"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="BoundaryEnhancementVolumeStyle">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html#BoundaryEnhancementVolumeStyle"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposableVolumeRenderStyleNode">
					<xs:choice minOccurs="0">
						<xs:annotation>
							<xs:documentation>transferFunction</xs:documentation>
						</xs:annotation>
						<xs:group ref="TextureContentModel"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="boundaryOpacity" default="0">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="opacityFactor" default="1">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="retainedOpacity" default="1">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
								<xs:maxInclusive value="1"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="CartoonVolumeStyle">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html#CartoonVolumeStyle"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposableVolumeRenderStyleNode">
					<xs:choice minOccurs="0">
						<xs:annotation>
							<xs:documentation>surfaceNormals</xs:documentation>
						</xs:annotation>
						<xs:group ref="Texture3DContentModel"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="colorSteps" default="4">
						<xs:simpleType>
							<xs:restriction base="SFInt32">
								<xs:minInclusive value="1"/>
								<xs:maxInclusive value="64"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="orthogonalColor" type="SFColorRGBA" default="1 1 1 1"/>
					<xs:attribute name="parallelColor" type="SFColorRGBA" default="0 0 0 1"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ComposedVolumeStyle">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html#ComposedVolumeStyle"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposableVolumeRenderStyleNode">
					<xs:choice minOccurs="0" maxOccurs="unbounded">
						<xs:annotation>
							<xs:documentation>renderStyle</xs:documentation>
						</xs:annotation>
						<xs:group ref="VolumeRenderStyleContentModel"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="ordered" type="SFBool" default="false"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="EdgeEnhancementVolumeStyle">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html#EdgeEnhancementVolumeStyle"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposableVolumeRenderStyleNode">
					<xs:choice minOccurs="0">
						<xs:annotation>
							<xs:documentation>surfaceNormals</xs:documentation>
						</xs:annotation>
						<xs:group ref="Texture3DContentModel"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="edgeColor" type="SFColorRGBA" default="0 0 0 1"/>
					<xs:attribute name="gradientThreshold" default="0.4">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
								<xs:maxInclusive value="3.141592653"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="IsoSurfaceVolumeData">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html#IsoSurfaceVolumeData"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DVolumeDataNode">
					<xs:sequence-cl minOccurs="0" maxOccurs="unbounded">
						<xs:group ref="Texture3DContentModel">
							<xs:annotation>
								<xs:documentation>gradients or voxels</xs:documentation>
							</xs:annotation>
						</xs:group>
						<xs:group ref="VolumeRenderStyleContentModel">
							<xs:annotation>
								<xs:documentation>MFNode renderStyle</xs:documentation>
							</xs:annotation>
						</xs:group>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:sequence-cl>
					<xs:attribute name="contourStepSize" type="SFFloat" default="0"/>
					<xs:attribute name="surfaceTolerance" default="0">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="surfaceValues" type="MFFloat"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="OpacityMapVolumeStyle">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html#OpacityMapVolumeStyle"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposableVolumeRenderStyleNode">
					<xs:choice minOccurs="0">
						<xs:annotation>
							<xs:documentation>transferFunction</xs:documentation>
						</xs:annotation>
						<xs:group ref="TextureContentModel"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ProjectionVolumeStyle">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html#ProjectionVolumeStyle"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DVolumeRenderStyleNode">
					<xs:attribute name="intensityThreshold" default="0">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
								<xs:maxInclusive value="1"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="type" default="MAX">
						<xs:simpleType>
							<xs:restriction base="SFString">
								<xs:enumeration value="MAX"/>
								<xs:enumeration value="MIN"/>
								<xs:enumeration value="AVERAGE"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="SegmentedVolumeData">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html#SegmentedVolumeData"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DVolumeDataNode">
					<xs:choice>
						<xs:group ref="Texture3DContentModel">
							<xs:annotation>
								<xs:documentation>SFNode segmentIdentifiers, SFNode voxels</xs:documentation>
							</xs:annotation>
						</xs:group>
						<xs:group ref="VolumeRenderStyleContentModel">
							<xs:annotation>
								<xs:documentation>MFNode renderStyle</xs:documentation>
							</xs:annotation>
						</xs:group>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="segmentEnabled" type="MFBool"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ShadedVolumeStyle">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html#ShadedVolumeStyle"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposableVolumeRenderStyleNode">
					<xs:choice>
						<xs:sequence-cl>
							<xs:element ref="Material">
								<xs:annotation>
									<xs:documentation>material</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:choice minOccurs="0">
								<xs:annotation>
									<xs:documentation>surfaceNormals</xs:documentation>
								</xs:annotation>
								<xs:group ref="Texture3DContentModel"/>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:group ref="Texture3DContentModel">
								<xs:annotation>
									<xs:documentation>surfaceNormals</xs:documentation>
								</xs:annotation>
							</xs:group>
							<xs:choice minOccurs="0">
								<xs:annotation>
									<xs:documentation>material</xs:documentation>
								</xs:annotation>
								<xs:element ref="Material"/>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>material or surfaceNormals</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:choice minOccurs="0">
								<xs:annotation>
									<xs:documentation>surfaceNormals or material</xs:documentation>
								</xs:annotation>
								<xs:group ref="Texture3DContentModel">
									<xs:annotation>
										<xs:documentation>surfaceNormals</xs:documentation>
									</xs:annotation>
								</xs:group>
								<xs:element ref="Material">
									<xs:annotation>
										<xs:documentation>material</xs:documentation>
									</xs:annotation>
								</xs:element>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
					</xs:choice>
					<xs:attribute name="lighting" type="SFBool" default="false"/>
					<xs:attribute name="shadows" type="SFBool" default="false"/>
					<xs:attribute name="phaseFunction" default="Henyey-Greenstein">
						<xs:simpleType>
							<xs:restriction base="SFString">
								<xs:enumeration value="Henyey-Greenstein"/>
								<xs:enumeration value="NONE"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="SilhouetteEnhancementVolumeStyle">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html#SilhouetteEnhancementVolumeStyle"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposableVolumeRenderStyleNode">
					<xs:choice minOccurs="0">
						<xs:annotation>
							<xs:documentation>surfaceNormals</xs:documentation>
						</xs:annotation>
						<xs:group ref="Texture3DContentModel"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="silhouetteBoundaryOpacity" default="0">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
								<xs:maxInclusive value="1"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="silhouetteRetainedOpacity" default="1">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
								<xs:maxInclusive value="1"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
					<xs:attribute name="silhouetteSharpness" default="0.5">
						<xs:simpleType>
							<xs:restriction base="SFFloat">
								<xs:minInclusive value="0"/>
							</xs:restriction>
						</xs:simpleType>
					</xs:attribute>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="ToneMappedVolumeStyle">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html#ToneMappedVolumeStyle"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DComposableVolumeRenderStyleNode">
					<xs:choice minOccurs="0">
						<xs:annotation>
							<xs:documentation>surfaceNormals</xs:documentation>
						</xs:annotation>
						<xs:group ref="Texture3DContentModel"/>
						<xs:element ref="ProtoInstance">
							<xs:annotation>
								<xs:documentation>Appropriately typed substitute node</xs:documentation>
							</xs:annotation>
						</xs:element>
					</xs:choice>
					<xs:attribute name="coolColor" type="SFColorRGBA" default="0 0 1 0"/>
					<xs:attribute name="warmColor" type="SFColorRGBA" default="1 1 0 0"/>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
	<xs:element name="VolumeData">
		<xs:annotation>
			<xs:documentation source="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html#VolumeData"/>
		</xs:annotation>
		<xs:complexType>
			<xs:complexContent>
				<xs:extension base="X3DVolumeDataNode">
					<xs:choice>
						<xs:sequence-cl>
							<xs:group ref="VolumeRenderStyleContentModel">
								<xs:annotation>
									<xs:documentation>renderStyle</xs:documentation>
								</xs:annotation>
							</xs:group>
							<xs:choice minOccurs="0">
								<xs:annotation>
									<xs:documentation>voxels</xs:documentation>
								</xs:annotation>
								<xs:group ref="Texture3DContentModel"/>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:group ref="Texture3DContentModel">
								<xs:annotation>
									<xs:documentation>voxels</xs:documentation>
								</xs:annotation>
							</xs:group>
							<xs:choice minOccurs="0">
								<xs:annotation>
									<xs:documentation>renderStyle</xs:documentation>
								</xs:annotation>
								<xs:group ref="VolumeRenderStyleContentModel"/>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
						<xs:sequence-cl>
							<xs:element ref="ProtoInstance">
								<xs:annotation>
									<xs:documentation>renderStyle or voxels</xs:documentation>
								</xs:annotation>
							</xs:element>
							<xs:choice minOccurs="0">
								<xs:annotation>
									<xs:documentation>voxels or renderStyle</xs:documentation>
								</xs:annotation>
								<xs:group ref="Texture3DContentModel">
									<xs:annotation>
										<xs:documentation>voxels</xs:documentation>
									</xs:annotation>
								</xs:group>
								<xs:group ref="VolumeRenderStyleContentModel">
									<xs:annotation>
										<xs:documentation>renderStyle</xs:documentation>
									</xs:annotation>
								</xs:group>
								<xs:element ref="ProtoInstance">
									<xs:annotation>
										<xs:documentation>Appropriately typed substitute node</xs:documentation>
									</xs:annotation>
								</xs:element>
							</xs:choice>
						</xs:sequence-cl>
					</xs:choice>
				</xs:extension>
			</xs:complexContent>
		</xs:complexType>
	</xs:element>
</xs:schema>
)
