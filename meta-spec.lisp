;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; meta-spec.lisp --- The specification of where different classes should be written
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

(in-package #:x3d)

;; ----------------------------------------------------------------------------
;; Code to handle auto creation of project

(defparameter *X3D-DOCUMENTATION-SPEC*
  `(
    (:name concepts
     :abstract (SceneGraphStructureNodeType
                X3DPrototype
                WildcardNodeType)
     :node (component
            EXPORT
            IMPORT
            IS
            connect
            field
            fieldValue
            head
            ExternProtoDeclare
            ProtoDeclare
            ProtoInterface
            ProtoBody
            ProtoInstance
            Route
            ;; Scene (hold this off for now)
            X3D
            ))
    (:name core
     :abstract  (X3DBindableNode
                 X3DChildNode
                 X3DInfoNode
                 X3DMetadataObject
                 X3DNode
                 X3DPrototypeInstance
                 X3DSensorNode)
     :node  (MetadataDouble
             MetadataFloat
             MetadataInteger
             MetadataSet
             MetadataString
             WorldInfo))
    (:name time
     :abstract  (X3DTimeDependentNode)
     :node  (TimeSensor))
    (:name networking
     :abstract  (X3DNetworkSensorNode
                 X3DUrlObject)
     :node  (Anchor
             Inline
             LoadSensor))
    (:name grouping
     :abstract  (X3DBoundedObject
                 X3DGroupingNode)
     :node  (Group
             StaticGroup
             Switch
             Transform))
    (:name rendering
     :abstract  (X3DColorNode
                 X3DComposedGeometryNode
                 X3DCoordinateNode
                 X3DGeometricPropertyNode
                 X3DGeometryNode
                 X3DNormalNode)
     :node  (ClipPlane
             Color
             ColorRGBA
             Coordinate
             IndexedLineSet
             IndexedTriangleFanSet
             IndexedTriangleSet
             IndexedTriangleStripSet
             LineSet
             Normal
             PointSet
             TriangleFanSet
             TriangleSet
             TriangleStripSet))
    (:name shape
     :abstract  (X3DAppearanceChildNode
                 X3DAppearanceNode
                 X3DMaterialNode
                 X3DShapeNode)
     :node  (Appearance
             FillProperties
             LineProperties
             Material
             Shape
             TwoSidedMaterial))
    (:name geometry-3d
     :abstract  ()
     :node   (Box
              Cone
              Cylinder
              ElevationGrid
              Extrusion
              IndexedFaceSet
              Sphere))
    (:name geometry-2d
     :abstract  ()
     :node  (Arc2D
             ArcClose2D
             Circle2D
             Disk2D
             Polyline2D
             Polypoint2D
             Rectangle2D
             TriangleSet2D))
    (:name text
     :abstract  (X3DFontStyleNode)
     :node  (FontStyle
             Text))
    (:name sound
     :abstract  (X3DSoundNode
                 X3DSoundSourceNode)
     :node  (AudioClip
             Sound))
    (:name lighting
     :abstract  (X3DLightNode)
     :node  (DirectionalLight
             PointLight
             SpotLight))
    (:name texturing
     :abstract  (X3DTextureCoordinateNode
                 X3DTextureNode
                 X3DTexture2DNode
                 X3DTextureTransformNode)
     :node  (ImageTexture
             MovieTexture
             MultiTexture
             MultiTextureCoordinate
             MultiTextureTransform
             PixelTexture
             TextureCoordinate
             TextureCoordinateGenerator
             TextureProperties
             TextureTransform))
    (:name interpolation
     :abstract  (X3DInterpolatorNode)
     :node  (ColorInterpolator
             CoordinateInterpolator
             CoordinateInterpolator2D
             EaseInEaseOut
             NormalInterpolator
             OrientationInterpolator
             PositionInterpolator
             PositionInterpolator2D
             ScalarInterpolator
             SplinePositionInterpolator
             SplinePositionInterpolator2D
             SplineScalarInterpolator
             SquadOrientationInterpolator))
    (:name pointing-device-sensor
     :abstract  (X3DDragSensorNode
                 X3DPointingDeviceSensorNode
                 X3DTouchSensorNode)
     :node  (CylinderSensor
             PlaneSensor
             SphereSensor
             TouchSensor))
    (:name key-device-sensor
     :abstract  (X3DKeyDeviceSensorNode)
     :node  (KeySensor
             StringSensor))
    (:name environment-sensor
     :abstract  (X3DEnvironmentalSensorNode)
     :node  (ProximitySensor
             TransformSensor
             VisibilitySensor))
    (:name navigation
     :abstract  (X3DViewpointNode)
     :node  (Billboard
             Collision
             LOD
             NavigationInfo
             OrthoViewpoint
             Viewpoint
             ViewpointGroup))
    (:name environmental-effects
     :abstract  (X3DBackgroundNode
                 X3DFogObject)
     :node  (Background
             Fog
             FogCoordinate
             LocalFog
             TextureBackground))
    (:name geospatial
     :abstract  ()
     :node  (GeoCoordinate
             GeoElevationGrid
             GeoLocation
             GeoLOD
             GeoMetadata
             GeoOrigin
             GeoPositionInterpolator
             GeoProximitySensor
             GeoTouchSensor
             GeoTransform
             GeoViewpoint))
    (:name humanoid-animation
     :abstract  ()
     :node  (HAnimDisplacer
             HAnimHumanoid
             HAnimJoint
             HAnimSegment
             HAnimSite))
    (:name NURBS
     :abstract  (X3DNurbsControlCurveNode
                 X3DNurbsSurfaceGeometryNode
                 X3DParametricGeometryNode)
     :node  (Contour2D
             ContourPolyline2D
             CoordinateDouble
             NurbsCurve
             NurbsCurve2D
             NurbsOrientationInterpolator
             NurbsPatchSurface
             NurbsPositionInterpolator
             NurbsSet
             NurbsSurfaceInterpolator
             NurbsSweptSurface
             NurbsSwungSurface
             NurbsTextureCoordinate
             NurbsTrimmedSurface))
    (:name distributed-interactive-simulation
     :abstract  ()
     :node  (DISEntityManager
             DISEntityTypeMapping
             EspduTransform
             ReceiverPdu
             SignalPdu
             TransmitterPdu))
    (:name scripting
     :abstract  (X3DScriptNode)
     :node  (Script))
    (:name event-utilities
     :abstract  (X3DSequencerNode
                 X3DTriggerNode)
     :node  (BooleanFilter
             BooleanSequencer
             BooleanToggle
             BooleanTrigger
             IntegerSequencer
             IntegerTrigger
             TimeTrigger))
    (:name programmable-shaders
     :abstract  (X3DProgrammableShaderObject
                 X3DShaderNode
                 X3DVertexAttributeNode)
     :node  (ComposedShader
             FloatVertexAttribute
             Matrix3VertexAttribute
             Matrix4VertexAttribute
             PackagedShader
             ProgramShader
             ShaderPart
             ShaderProgram))
    (:name CAD-geometry
     :abstract  (X3DProductStructureChildNode)
     :node  (CADAssembly
             CADFace
             CADLayer
             CADPart
             IndexedQuadSet
             QuadSet))
    (:name texturing-3d
     :abstract  (X3DTexture3DNode)
     :node  (ComposedTexture3D
             ImageTexture3D
             PixelTexture3D
             TextureCoordinate3D
             TextureCoordinate4D
             TextureTransformMatrix3D
             TextureTransform3D))
    (:name cube-map-environmental-texturing
     :abstract  (X3DEnvironmentTextureNode)
     :node  (ComposedCubeMapTexture
             GeneratedCubeMapTexture
             ImageCubeMapTexture))
    (:name layering
     :abstract  (X3DLayerNode
                 X3DViewportNode)
     :node  (Layer
             LayerSet
             Viewport))
    (:name layout
     :abstract  (X3DLayoutNode)
     :node  (Layout
             LayoutGroup
             LayoutLayer
             ScreenFontStyle
             ScreenGroup))
    (:name ridgid-body-physics
     :abstract  (X3DNBodyCollidableNode
                 X3DNBodyCollisionSpaceNode
                 X3DRigidJointNode)
     :node  (BallJoint
             CollidableOffset
             CollidableShape
             CollisionCollection
             CollisionSensor
             CollisionSpace
             Contact
             DoubleAxisHingeJoint
             MotorJoint
             RigidBody
             RigidBodyCollection
             SingleAxisHingeJoint
             SliderJoint
             UniversalJoint))
    (:name picking-sensor
     :abstract  (X3DPickableObject
                 X3DPickSensorNode)
     :node  (LinePickSensor
             PickableGroup
             PointPickSensor
             PrimitivePickSensor
             VolumePickSensor))
    (:name follower
     :abstract  (X3DChaserNode
                 X3DDamperNode
                 X3DFollowerNode)
     :node  (ColorDamper
             CoordinateDamper
             OrientationChaser
             OrientationDamper
             PositionChaser
             PositionChaser2D
             PositionDamper
             PositionDamper2D
             ScalarChaser
             TexCoordDamper2D))
    (:name particle-systems
     :abstract  (X3DParticleEmitterNode
                 X3DParticlePhysicsModelNode)
     :node  (BoundedPhysicsModel
             ConeEmitter
             ExplosionEmitter
             ForcePhysicsModel
             ParticleSystem
             PointEmitter
             PolylineEmitter
             SurfaceEmitter
             VolumeEmitter
             WindPhysicsModel))))

