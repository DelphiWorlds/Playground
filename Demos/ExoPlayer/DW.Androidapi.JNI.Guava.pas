unit DW.Androidapi.JNI.Guava;

interface

uses
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes;

type
  JAbstractMultimap = interface;
  JBaseImmutableMultimap = interface;
  JImmutableCollection = interface;
  JImmutableCollection_ArrayBasedBuilder = interface;
  JImmutableCollection_Builder = interface;
  JImmutableList = interface;
  JImmutableList_Builder = interface;
  JImmutableMap = interface;
  JImmutableMap_Builder = interface;
  JImmutableMultimap = interface;
  JImmutableMultimap_Builder = interface;
  JImmutableMultiset = interface;
  JImmutableMultisetGwtSerializationDependencies = interface;
  JImmutableMultiset_Builder = interface;
  JImmutableSet = interface;
  JImmutableSetMultimap = interface;
  JImmutableSetMultimap_Builder = interface;
  JImmutableSet_Builder = interface;
  JMultimap = interface;
  JMultiset = interface;
  JUnmodifiableIterator = interface;
  JUnmodifiableListIterator = interface;

  JUnmodifiableIteratorClass = interface(JObjectClass)
    ['{963B3D19-95D6-4728-9524-6F0D848796F6}']
  end;

  [JavaSignature('com/google/common/collect/UnmodifiableIterator')]
  JUnmodifiableIterator = interface(JObject)
    ['{155E4861-A897-4765-9A8A-A7A416DABA11}']
    procedure remove; cdecl;
  end;
  TJUnmodifiableIterator = class(TJavaGenericImport<JUnmodifiableIteratorClass, JUnmodifiableIterator>) end;

  JMultisetClass = interface(JCollectionClass)
    ['{A8751B84-E4C1-42BB-923B-CB0B366F40D4}']
  end;

  [JavaSignature('com/google/common/collect/Multiset')]
  JMultiset = interface(JCollection)
    ['{1F3403C7-FE44-44C1-8789-DF79D2ECEB0A}']
    function add(e: JObject): Boolean; cdecl; overload;
    function add(e: JObject; int: Integer): Integer; cdecl; overload;
    function contains(object_1: JObject): Boolean; cdecl;
    function containsAll(collection: JCollection): Boolean; cdecl;
    function count(object_1: JObject): Integer; cdecl;
    function elementSet: JSet; cdecl;
    function entrySet: JSet; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function iterator: JIterator; cdecl;
    function remove(object_1: JObject; int: Integer): Integer; cdecl; overload;
    function remove(object_1: JObject): Boolean; cdecl; overload;
    function removeAll(collection: JCollection): Boolean; cdecl;
    function retainAll(collection: JCollection): Boolean; cdecl;
    function setCount(e: JObject; int: Integer; int_1: Integer): Boolean; cdecl; overload;
    function setCount(e: JObject; int: Integer): Integer; cdecl; overload;
    function size: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJMultiset = class(TJavaGenericImport<JMultisetClass, JMultiset>) end;

  JMultimapClass = interface(IJavaClass)
    ['{1A06E69F-FBD8-4703-B621-A5B0507195F9}']
  end;

  [JavaSignature('com/google/common/collect/Multimap')]
  JMultimap = interface(IJavaInstance)
    ['{38A6B27D-4E71-4249-A705-2CA8B4B1582C}']
    function asMap: JMap; cdecl;
    procedure clear; cdecl;
    function containsEntry(object_1: JObject; object_2: JObject): Boolean; cdecl;
    function containsKey(object_1: JObject): Boolean; cdecl;
    function containsValue(object_1: JObject): Boolean; cdecl;
    function entries: JCollection; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function get(k: JObject): JCollection; cdecl;
    function hashCode: Integer; cdecl;
    function isEmpty: Boolean; cdecl;
    function keySet: JSet; cdecl;
    function keys: JMultiset; cdecl;
    function put(k: JObject; v: JObject): Boolean; cdecl;
    function putAll(k: JObject; iterable: JIterable): Boolean; cdecl; overload;
    function putAll(multimap: JMultimap): Boolean; cdecl; overload;
    function remove(object_1: JObject; object_2: JObject): Boolean; cdecl;
    function removeAll(object_1: JObject): JCollection; cdecl;
    function replaceValues(k: JObject; iterable: JIterable): JCollection; cdecl;
    function size: Integer; cdecl;
    function values: JCollection; cdecl;
  end;
  TJMultimap = class(TJavaGenericImport<JMultimapClass, JMultimap>) end;

  JImmutableMultimap_BuilderClass = interface(JObjectClass)
    ['{BA9A837B-3195-4E41-8BBD-4E169322FCAF}']
    {class} function init: JImmutableMultimap_Builder; cdecl;
  end;

  [JavaSignature('com/google/common/collect/ImmutableMultimap$Builder')]
  JImmutableMultimap_Builder = interface(JObject)
    ['{6D1D3B00-D323-44AB-9865-0E85FF7EB78D}']
    function build: JImmutableMultimap; cdecl;
    function orderKeysBy(comparator: JComparator): JImmutableMultimap_Builder; cdecl;
    function orderValuesBy(comparator: JComparator): JImmutableMultimap_Builder; cdecl;
    function put(entry: JMap_Entry): JImmutableMultimap_Builder; cdecl; overload;
    function put(k: JObject; v: JObject): JImmutableMultimap_Builder; cdecl; overload;
    function putAll(multimap: JMultimap): JImmutableMultimap_Builder; cdecl; overload;
    function putAll(iterable: JIterable): JImmutableMultimap_Builder; cdecl; overload;
    function putAll(k: JObject; iterable: JIterable): JImmutableMultimap_Builder; cdecl; overload;
    function putAll(k: JObject; v: JObject): JImmutableMultimap_Builder; cdecl; overload;
  end;
  TJImmutableMultimap_Builder = class(TJavaGenericImport<JImmutableMultimap_BuilderClass, JImmutableMultimap_Builder>) end;

  JImmutableMapClass = interface(JObjectClass)
    ['{2B06CBB8-75E2-4573-98CE-54C72A2A0CB3}']
    {class} function builder: JImmutableMap_Builder; cdecl;
    {class} function builderWithExpectedSize(int: Integer): JImmutableMap_Builder; cdecl;
    {class} function copyOf(map: JMap): JImmutableMap; cdecl; overload;
    {class} function copyOf(iterable: JIterable): JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject): JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject): JImmutableMap; cdecl; overload;
    {class} function &of: JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject; k_4: JObject; v_4: JObject; k_5: JObject; v_5: JObject): JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject; k_4: JObject; v_4: JObject; k_5: JObject; v_5: JObject; k_6: JObject; v_6: JObject): JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject; k_4: JObject; v_4: JObject; k_5: JObject; v_5: JObject; k_6: JObject; v_6: JObject; k_7: JObject; v_7: JObject; k_8: JObject; v_8: JObject; k_9: JObject; v_9: JObject): JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject; k_4: JObject; v_4: JObject; k_5: JObject; v_5: JObject; k_6: JObject; v_6: JObject; k_7: JObject; v_7: JObject; k_8: JObject; v_8: JObject): JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject; k_4: JObject; v_4: JObject; k_5: JObject; v_5: JObject; k_6: JObject; v_6: JObject; k_7: JObject; v_7: JObject): JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject): JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject): JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject; k_4: JObject; v_4: JObject): JImmutableMap; cdecl; overload;
    {class} function ofEntries(entry: JMap_Entry): JImmutableMap; cdecl;
  end;

  [JavaSignature('com/google/common/collect/ImmutableMap')]
  JImmutableMap = interface(JObject)
    ['{96D01E10-081B-469B-9C7B-5C36599AF9DE}']
    function asMultimap: JImmutableSetMultimap; cdecl;
    procedure clear; cdecl;
    function containsKey(object_1: JObject): Boolean; cdecl;
    function containsValue(object_1: JObject): Boolean; cdecl;
//    function entrySet: JImmutableSet; cdecl; overload;
//    function entrySet: JSet; cdecl; overload;
    function equals(object_1: JObject): Boolean; cdecl;
    function get(object_1: JObject): JObject; cdecl;
    function getOrDefault(object_1: JObject; v: JObject): JObject; cdecl;
    function hashCode: Integer; cdecl;
    function isEmpty: Boolean; cdecl;
//    function keySet: JImmutableSet; cdecl; overload;
//    function keySet: JSet; cdecl; overload;
    function put(k: JObject; v: JObject): JObject; cdecl;
    procedure putAll(map: JMap); cdecl;
    function remove(object_1: JObject): JObject; cdecl;
    function toString: JString; cdecl;
//    function values: JCollection; cdecl; overload;
//    function values: JImmutableCollection; cdecl; overload;
  end;
  TJImmutableMap = class(TJavaGenericImport<JImmutableMapClass, JImmutableMap>) end;

  JImmutableMap_BuilderClass = interface(JObjectClass)
    ['{DFE5C31D-639B-4244-B879-228635867475}']
    {class} function init: JImmutableMap_Builder; cdecl;
  end;

  [JavaSignature('com/google/common/collect/ImmutableMap$Builder')]
  JImmutableMap_Builder = interface(JObject)
    ['{033D20AC-55E8-4E7C-BB59-361E59E7B2B1}']
    function build: JImmutableMap; cdecl;
    function buildKeepingLast: JImmutableMap; cdecl;
    function buildOrThrow: JImmutableMap; cdecl;
    function orderEntriesByValue(comparator: JComparator): JImmutableMap_Builder; cdecl;
    function put(entry: JMap_Entry): JImmutableMap_Builder; cdecl; overload;
    function put(k: JObject; v: JObject): JImmutableMap_Builder; cdecl; overload;
    function putAll(iterable: JIterable): JImmutableMap_Builder; cdecl; overload;
    function putAll(map: JMap): JImmutableMap_Builder; cdecl; overload;
  end;
  TJImmutableMap_Builder = class(TJavaGenericImport<JImmutableMap_BuilderClass, JImmutableMap_Builder>) end;

  JImmutableCollectionClass = interface(JAbstractCollectionClass)
    ['{6B413A6E-24A4-4DC5-81F3-D870182D83A6}']
  end;

  [JavaSignature('com/google/common/collect/ImmutableCollection')]
  JImmutableCollection = interface(JAbstractCollection)
    ['{47720155-A166-40A9-862F-24ABF35AF2BB}']
    function add(e: JObject): Boolean; cdecl;
    function addAll(collection: JCollection): Boolean; cdecl;
    function asList: JImmutableList; cdecl;
    procedure clear; cdecl;
    function contains(object_1: JObject): Boolean; cdecl;
//    function iterator: JUnmodifiableIterator; cdecl; overload;
//    function iterator: JIterator; cdecl; overload;
    function remove(object_1: JObject): Boolean; cdecl;
    function removeAll(collection: JCollection): Boolean; cdecl;
    function retainAll(collection: JCollection): Boolean; cdecl;
    function toArray: TJavaObjectArray<JObject>; cdecl; overload;
    function toArray(ts: TJavaObjectArray<JObject>): TJavaObjectArray<JObject>; cdecl; overload;
  end;
  TJImmutableCollection = class(TJavaGenericImport<JImmutableCollectionClass, JImmutableCollection>) end;

  JImmutableCollection_BuilderClass = interface(JObjectClass)
    ['{780844D8-51A8-4C16-96CD-168745B463B7}']
  end;

  [JavaSignature('com/google/common/collect/ImmutableCollection$Builder')]
  JImmutableCollection_Builder = interface(JObject)
    ['{33FC8916-A109-4E11-8773-5BD4E8B456EF}']
//    function add(e: JObject): JImmutableCollection_Builder; cdecl; overload;
//    function add(e: JObject): JImmutableCollection_Builder; cdecl; overload;
    function addAll(iterator: JIterator): JImmutableCollection_Builder; cdecl; overload;
    function addAll(iterable: JIterable): JImmutableCollection_Builder; cdecl; overload;
    function build: JImmutableCollection; cdecl;
  end;
  TJImmutableCollection_Builder = class(TJavaGenericImport<JImmutableCollection_BuilderClass, JImmutableCollection_Builder>) end;

  JImmutableCollection_ArrayBasedBuilderClass = interface(JImmutableCollection_BuilderClass)
    ['{4B1DD1F2-C780-488D-BC39-0C9FB248374E}']
  end;

  [JavaSignature('com/google/common/collect/ImmutableCollection$ArrayBasedBuilder')]
  JImmutableCollection_ArrayBasedBuilder = interface(JImmutableCollection_Builder)
    ['{0326428D-32DC-4512-A7B2-04ACA7D5A594}']
//    function add(object_1: JObject): JImmutableCollection_Builder; cdecl; overload;
//    function add(e: JObject): JImmutableCollection_Builder; cdecl; overload;
//    function add(e: JObject): JImmutableCollection_ArrayBasedBuilder; cdecl; overload;
    function addAll(iterable: JIterable): JImmutableCollection_Builder; cdecl;
  end;
  TJImmutableCollection_ArrayBasedBuilder = class(TJavaGenericImport<JImmutableCollection_ArrayBasedBuilderClass, JImmutableCollection_ArrayBasedBuilder>) end;

  JAbstractMultimapClass = interface(JObjectClass)
    ['{F5627A6A-79D6-4287-8698-17C05957662F}']
  end;

  [JavaSignature('com/google/common/collect/AbstractMultimap')]
  JAbstractMultimap = interface(JObject)
    ['{8F53BAA7-BBD3-445D-BADA-BCF033BBF25F}']
    function asMap: JMap; cdecl;
    function containsEntry(object_1: JObject; object_2: JObject): Boolean; cdecl;
    function containsValue(object_1: JObject): Boolean; cdecl;
    function entries: JCollection; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function isEmpty: Boolean; cdecl;
    function keySet: JSet; cdecl;
    function keys: JMultiset; cdecl;
    function put(k: JObject; v: JObject): Boolean; cdecl;
    function putAll(k: JObject; iterable: JIterable): Boolean; cdecl; overload;
    function putAll(multimap: JMultimap): Boolean; cdecl; overload;
    function remove(object_1: JObject; object_2: JObject): Boolean; cdecl;
    function replaceValues(k: JObject; iterable: JIterable): JCollection; cdecl;
    function toString: JString; cdecl;
    function values: JCollection; cdecl;
  end;
  TJAbstractMultimap = class(TJavaGenericImport<JAbstractMultimapClass, JAbstractMultimap>) end;

  JUnmodifiableListIteratorClass = interface(JUnmodifiableIteratorClass)
    ['{7BDDFC50-47F5-4421-B531-FB276A668F21}']
  end;

  [JavaSignature('com/google/common/collect/UnmodifiableListIterator')]
  JUnmodifiableListIterator = interface(JUnmodifiableIterator)
    ['{50EAC0E7-29B8-4F1A-9FE5-A79F8E2AC866}']
    procedure add(e: JObject); cdecl;
    procedure &set(e: JObject); cdecl;
  end;
  TJUnmodifiableListIterator = class(TJavaGenericImport<JUnmodifiableListIteratorClass, JUnmodifiableListIterator>) end;

  JImmutableSetMultimap_BuilderClass = interface(JImmutableMultimap_BuilderClass)
    ['{B7DA8212-99BC-482B-AF97-786E197D6AAF}']
    {class} function init: JImmutableSetMultimap_Builder; cdecl;
  end;

  [JavaSignature('com/google/common/collect/ImmutableSetMultimap$Builder')]
  JImmutableSetMultimap_Builder = interface(JImmutableMultimap_Builder)
    ['{49142BB6-1A8F-4324-9CD2-E157A6ED8F44}']
//    function build: JImmutableMultimap; cdecl; overload;
//    function build: JImmutableSetMultimap; cdecl; overload;
//    function orderKeysBy(comparator: JComparator): JImmutableMultimap_Builder; cdecl; overload;
//    function orderKeysBy(comparator: JComparator): JImmutableSetMultimap_Builder; cdecl; overload;
//    function orderValuesBy(comparator: JComparator): JImmutableSetMultimap_Builder; cdecl; overload;
//    function orderValuesBy(comparator: JComparator): JImmutableMultimap_Builder; cdecl; overload;
//    function put(entry: JMap_Entry): JImmutableSetMultimap_Builder; cdecl; overload;
//    function put(object_1: JObject; object_2: JObject): JImmutableMultimap_Builder; cdecl; overload;
//    function put(entry: JMap_Entry): JImmutableMultimap_Builder; cdecl; overload;
//    function put(k: JObject; v: JObject): JImmutableSetMultimap_Builder; cdecl; overload;
//    function putAll(iterable: JIterable): JImmutableMultimap_Builder; cdecl; overload;
//    function putAll(object_1: JObject; iterable: JIterable): JImmutableMultimap_Builder; cdecl; overload;
//    function putAll(k: JObject; iterable: JIterable): JImmutableSetMultimap_Builder; cdecl; overload;
//    function putAll(iterable: JIterable): JImmutableSetMultimap_Builder; cdecl; overload;
//    function putAll(multimap: JMultimap): JImmutableSetMultimap_Builder; cdecl; overload;
//    function putAll(k: JObject; v: JObject): JImmutableSetMultimap_Builder; cdecl; overload;
//    function putAll(multimap: JMultimap): JImmutableMultimap_Builder; cdecl; overload;
//    function putAll(object_1: JObject; objects: TJavaObjectArray<JObject>): JImmutableMultimap_Builder; cdecl; overload;
  end;
  TJImmutableSetMultimap_Builder = class(TJavaGenericImport<JImmutableSetMultimap_BuilderClass, JImmutableSetMultimap_Builder>) end;

  JImmutableSetClass = interface(JImmutableCollectionClass)
    ['{60CF8A13-64AB-4BC6-A418-8733107A526B}']
    {class} function builder: JImmutableSet_Builder; cdecl;
    {class} function builderWithExpectedSize(int: Integer): JImmutableSet_Builder; cdecl;
    {class} function copyOf(collection: JCollection): JImmutableSet; cdecl; overload;
    {class} function copyOf(iterable: JIterable): JImmutableSet; cdecl; overload;
    {class} function copyOf(iterator: JIterator): JImmutableSet; cdecl; overload;
    {class} function copyOf(es: TJavaObjectArray<JObject>): JImmutableSet; cdecl; overload;
    {class} function &of(e: JObject): JImmutableSet; cdecl; overload;
    {class} function &of: JImmutableSet; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject): JImmutableSet; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject): JImmutableSet; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject): JImmutableSet; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject; e_6: JObject): JImmutableSet; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject): JImmutableSet; cdecl; overload;
  end;

  [JavaSignature('com/google/common/collect/ImmutableSet')]
  JImmutableSet = interface(JImmutableCollection)
    ['{D9FEB25E-F9DE-4EE4-93B2-21298AE4028E}']
    function asList: JImmutableList; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
//    function iterator: JIterator; cdecl; overload;
//    function iterator: JUnmodifiableIterator; cdecl; overload;
  end;
  TJImmutableSet = class(TJavaGenericImport<JImmutableSetClass, JImmutableSet>) end;

  JImmutableSet_BuilderClass = interface(JImmutableCollection_ArrayBasedBuilderClass)
    ['{E8B7DAD2-6199-4388-9FCF-959E876C9870}']
    {class} function init: JImmutableSet_Builder; cdecl;
  end;

  [JavaSignature('com/google/common/collect/ImmutableSet$Builder')]
  JImmutableSet_Builder = interface(JImmutableCollection_ArrayBasedBuilder)
    ['{ECD2E4D0-D5DD-4409-8E81-76962375A4B6}']
//    function add(objects: TJavaObjectArray<JObject>): JImmutableCollection_Builder; cdecl; overload;
//    function add(object_1: JObject): JImmutableCollection_ArrayBasedBuilder; cdecl; overload;
//    function add(e: JObject): JImmutableSet_Builder; cdecl; overload;
//    function add(object_1: JObject): JImmutableCollection_Builder; cdecl; overload;
//    function add(e: JObject): JImmutableSet_Builder; cdecl; overload;
//    function addAll(iterable: JIterable): JImmutableCollection_Builder; cdecl; overload;
//    function addAll(iterable: JIterable): JImmutableSet_Builder; cdecl; overload;
//    function addAll(iterator: JIterator): JImmutableSet_Builder; cdecl; overload;
//    function addAll(iterator: JIterator): JImmutableCollection_Builder; cdecl; overload;
//    function build: JImmutableCollection; cdecl; overload;
//    function build: JImmutableSet; cdecl; overload;
  end;
  TJImmutableSet_Builder = class(TJavaGenericImport<JImmutableSet_BuilderClass, JImmutableSet_Builder>) end;

  JImmutableMultisetGwtSerializationDependenciesClass = interface(JImmutableCollectionClass)
    ['{DEF3A3D4-804F-4383-A4E0-A4C4DDEB88DA}']
  end;

  [JavaSignature('com/google/common/collect/ImmutableMultisetGwtSerializationDependencies')]
  JImmutableMultisetGwtSerializationDependencies = interface(JImmutableCollection)
    ['{C004A7D0-D2A4-47E8-9B90-CBFE602B6BA2}']
  end;
  TJImmutableMultisetGwtSerializationDependencies = class(TJavaGenericImport<JImmutableMultisetGwtSerializationDependenciesClass, JImmutableMultisetGwtSerializationDependencies>) end;

  JImmutableMultisetClass = interface(JImmutableMultisetGwtSerializationDependenciesClass)
    ['{1080CDB1-44AF-4CA0-8B0A-3AD25D96162C}']
    {class} function builder: JImmutableMultiset_Builder; cdecl;
    {class} function copyOf(es: TJavaObjectArray<JObject>): JImmutableMultiset; cdecl; overload;
    {class} function copyOf(iterable: JIterable): JImmutableMultiset; cdecl; overload;
    {class} function copyOf(iterator: JIterator): JImmutableMultiset; cdecl; overload;
    {class} function &of: JImmutableMultiset; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject; e_6: JObject): JImmutableMultiset; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject): JImmutableMultiset; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject): JImmutableMultiset; cdecl; overload;
    {class} function &of(e: JObject): JImmutableMultiset; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject): JImmutableMultiset; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject): JImmutableMultiset; cdecl; overload;
  end;

  [JavaSignature('com/google/common/collect/ImmutableMultiset')]
  JImmutableMultiset = interface(JImmutableMultisetGwtSerializationDependencies)
    ['{54EBC2DC-B68C-429B-9E6C-FE60B88DEC82}']
    function add(e: JObject; int: Integer): Integer; cdecl;
    function asList: JImmutableList; cdecl;
    function contains(object_1: JObject): Boolean; cdecl;
//    function elementSet: JSet; cdecl; overload;
//    function elementSet: JImmutableSet; cdecl; overload;
//    function entrySet: JImmutableSet; cdecl; overload;
//    function entrySet: JSet; cdecl; overload;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
//    function iterator: JUnmodifiableIterator; cdecl; overload;
//    function iterator: JIterator; cdecl; overload;
    function remove(object_1: JObject; int: Integer): Integer; cdecl;
    function setCount(e: JObject; int: Integer): Integer; cdecl; overload;
    function setCount(e: JObject; int: Integer; int_1: Integer): Boolean; cdecl; overload;
    function toString: JString; cdecl;
  end;
  TJImmutableMultiset = class(TJavaGenericImport<JImmutableMultisetClass, JImmutableMultiset>) end;

  JImmutableMultiset_BuilderClass = interface(JImmutableCollection_BuilderClass)
    ['{B0482750-7280-4CA7-A3BD-769BA5B3AE13}']
    {class} function init: JImmutableMultiset_Builder; cdecl;
  end;

  [JavaSignature('com/google/common/collect/ImmutableMultiset$Builder')]
  JImmutableMultiset_Builder = interface(JImmutableCollection_Builder)
    ['{E65E1FBA-4740-4CBD-A759-D03D0E80C8DE}']
//    function add(objects: TJavaObjectArray<JObject>): JImmutableCollection_Builder; cdecl; overload;
//    function add(e: JObject): JImmutableMultiset_Builder; cdecl; overload;
//    function add(object_1: JObject): JImmutableCollection_Builder; cdecl; overload;
//    function add(e: JObject): JImmutableMultiset_Builder; cdecl; overload;
//    function addAll(iterator: JIterator): JImmutableCollection_Builder; cdecl; overload;
//    function addAll(iterable: JIterable): JImmutableCollection_Builder; cdecl; overload;
//    function addAll(iterator: JIterator): JImmutableMultiset_Builder; cdecl; overload;
//    function addAll(iterable: JIterable): JImmutableMultiset_Builder; cdecl; overload;
    function addCopies(e: JObject; int: Integer): JImmutableMultiset_Builder; cdecl;
//    function build: JImmutableMultiset; cdecl; overload;
//    function build: JImmutableCollection; cdecl; overload;
    function setCount(e: JObject; int: Integer): JImmutableMultiset_Builder; cdecl;
  end;
  TJImmutableMultiset_Builder = class(TJavaGenericImport<JImmutableMultiset_BuilderClass, JImmutableMultiset_Builder>) end;

  JImmutableListClass = interface(JImmutableCollectionClass)
    ['{F56266A0-8CB3-4235-B0F8-6C4869F1C449}']
    {class} function builder: JImmutableList_Builder; cdecl;
    {class} function builderWithExpectedSize(int: Integer): JImmutableList_Builder; cdecl;
    {class} function copyOf(iterable: JIterable): JImmutableList; cdecl; overload;
    {class} function copyOf(iterator: JIterator): JImmutableList; cdecl; overload;
    {class} function copyOf(es: TJavaObjectArray<JObject>): JImmutableList; cdecl; overload;
    {class} function copyOf(collection: JCollection): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject; e_6: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject; e_6: JObject; e_7: JObject; e_8: JObject; e_9: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject; e_6: JObject; e_7: JObject; e_8: JObject; e_9: JObject; e_10: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject; e_6: JObject; e_7: JObject; e_8: JObject; e_9: JObject; e_10: JObject; e_11: JObject; e_12: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject; e_6: JObject; e_7: JObject; e_8: JObject): JImmutableList; cdecl; overload;
    {class} function &of: JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject; e_6: JObject; e_7: JObject): JImmutableList; cdecl; overload;
    {class} function sortedCopyOf(comparator: JComparator; iterable: JIterable): JImmutableList; cdecl; overload;
    {class} function sortedCopyOf(iterable: JIterable): JImmutableList; cdecl; overload;
  end;

  [JavaSignature('com/google/common/collect/ImmutableList')]
  JImmutableList = interface(JImmutableCollection)
    ['{A5A8ED33-65AD-4C0D-BC2D-F714A5F6606F}']
    procedure add(int: Integer; e: JObject); cdecl;
    function addAll(int: Integer; collection: JCollection): Boolean; cdecl;
    function asList: JImmutableList; cdecl;
    function contains(object_1: JObject): Boolean; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function indexOf(object_1: JObject): Integer; cdecl;
//    function iterator: JUnmodifiableIterator; cdecl; overload;
//    function iterator: JIterator; cdecl; overload;
    function lastIndexOf(object_1: JObject): Integer; cdecl;
//    function listIterator: JUnmodifiableListIterator; cdecl; overload;
//    function listIterator(int: Integer): JUnmodifiableListIterator; cdecl; overload;
//    function listIterator(int: Integer): JListIterator; cdecl; overload;
//    function listIterator: JListIterator; cdecl; overload;
    function remove(int: Integer): JObject; cdecl;
    function reverse: JImmutableList; cdecl;
    function &set(int: Integer; e: JObject): JObject; cdecl;
//    function subList(int: Integer; int_1: Integer): JList; cdecl; overload;
//    function subList(int: Integer; int_1: Integer): JImmutableList; cdecl; overload;
  end;
  TJImmutableList = class(TJavaGenericImport<JImmutableListClass, JImmutableList>) end;

  JImmutableList_BuilderClass = interface(JImmutableCollection_ArrayBasedBuilderClass)
    ['{13B150B7-24E6-4D06-8CFA-D68F9F841FC5}']
    {class} function init: JImmutableList_Builder; cdecl;
  end;

  [JavaSignature('com/google/common/collect/ImmutableList$Builder')]
  JImmutableList_Builder = interface(JImmutableCollection_ArrayBasedBuilder)
    ['{8D206C17-CA69-4EB7-ABDE-C9D379B0F978}']
//    function add(objects: TJavaObjectArray<JObject>): JImmutableCollection_Builder; cdecl; overload;
//    function add(object_1: JObject): JImmutableCollection_ArrayBasedBuilder; cdecl; overload;
//    function add(e: JObject): JImmutableList_Builder; cdecl; overload;
//    function add(object_1: JObject): JImmutableCollection_Builder; cdecl; overload;
//    function add(e: JObject): JImmutableList_Builder; cdecl; overload;
//    function addAll(iterable: JIterable): JImmutableCollection_Builder; cdecl; overload;
//    function addAll(iterable: JIterable): JImmutableList_Builder; cdecl; overload;
//    function addAll(iterator: JIterator): JImmutableList_Builder; cdecl; overload;
//    function addAll(iterator: JIterator): JImmutableCollection_Builder; cdecl; overload;
//    function build: JImmutableCollection; cdecl; overload;
//    function build: JImmutableList; cdecl; overload;
  end;
  TJImmutableList_Builder = class(TJavaGenericImport<JImmutableList_BuilderClass, JImmutableList_Builder>) end;

  JBaseImmutableMultimapClass = interface(JAbstractMultimapClass)
    ['{C1887F2D-2D65-46F4-B161-44B759576C5B}']
  end;

  [JavaSignature('com/google/common/collect/BaseImmutableMultimap')]
  JBaseImmutableMultimap = interface(JAbstractMultimap)
    ['{33167A7A-5026-46D7-B235-809DD2B66A6F}']
  end;
  TJBaseImmutableMultimap = class(TJavaGenericImport<JBaseImmutableMultimapClass, JBaseImmutableMultimap>) end;

  JImmutableMultimapClass = interface(JBaseImmutableMultimapClass)
    ['{AFDDA3A4-4A1E-475B-B2D9-6B7A8943A4E5}']
    {class} function builder: JImmutableMultimap_Builder; cdecl;
    {class} function copyOf(iterable: JIterable): JImmutableMultimap; cdecl; overload;
    {class} function copyOf(multimap: JMultimap): JImmutableMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject): JImmutableMultimap; cdecl; overload;
    {class} function &of: JImmutableMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject): JImmutableMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject): JImmutableMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject; k_4: JObject; v_4: JObject): JImmutableMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject): JImmutableMultimap; cdecl; overload;
  end;

  [JavaSignature('com/google/common/collect/ImmutableMultimap')]
  JImmutableMultimap = interface(JBaseImmutableMultimap)
    ['{072AB637-8353-441A-B2CC-59AED7615C4E}']
//    function asMap: JImmutableMap; cdecl; overload;
//    function asMap: JMap; cdecl; overload;
    procedure clear; cdecl;
    function containsEntry(object_1: JObject; object_2: JObject): Boolean; cdecl;
    function containsKey(object_1: JObject): Boolean; cdecl;
    function containsValue(object_1: JObject): Boolean; cdecl;
//    function entries: JCollection; cdecl; overload;
//    function entries: JImmutableCollection; cdecl; overload;
    function equals(object_1: JObject): Boolean; cdecl;
//    function get(k: JObject): JImmutableCollection; cdecl; overload;
//    function get(object_1: JObject): JCollection; cdecl; overload;
    function hashCode: Integer; cdecl;
    function inverse: JImmutableMultimap; cdecl;
    function isEmpty: Boolean; cdecl;
//    function keySet: JSet; cdecl; overload;
//    function keySet: JImmutableSet; cdecl; overload;
//    function keys: JImmutableMultiset; cdecl; overload;
//    function keys: JMultiset; cdecl; overload;
    function put(k: JObject; v: JObject): Boolean; cdecl;
    function putAll(multimap: JMultimap): Boolean; cdecl; overload;
    function putAll(k: JObject; iterable: JIterable): Boolean; cdecl; overload;
    function remove(object_1: JObject; object_2: JObject): Boolean; cdecl;
//    function removeAll(object_1: JObject): JCollection; cdecl; overload;
//    function removeAll(object_1: JObject): JImmutableCollection; cdecl; overload;
//    function replaceValues(k: JObject; iterable: JIterable): JImmutableCollection; cdecl; overload;
//    function replaceValues(object_1: JObject; iterable: JIterable): JCollection; cdecl; overload;
    function size: Integer; cdecl;
    function toString: JString; cdecl;
//    function values: JCollection; cdecl; overload;
//    function values: JImmutableCollection; cdecl; overload;
  end;
  TJImmutableMultimap = class(TJavaGenericImport<JImmutableMultimapClass, JImmutableMultimap>) end;

  JImmutableSetMultimapClass = interface(JImmutableMultimapClass)
    ['{54EC2A7F-FCFE-4479-B251-EAD1BF3C78AA}']
    {class} function builder: JImmutableSetMultimap_Builder; cdecl;
    {class} function copyOf(iterable: JIterable): JImmutableSetMultimap; cdecl; overload;
    {class} function copyOf(multimap: JMultimap): JImmutableSetMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject): JImmutableSetMultimap; cdecl; overload;
    {class} function &of: JImmutableSetMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject): JImmutableSetMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject): JImmutableSetMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject): JImmutableSetMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject; k_4: JObject; v_4: JObject): JImmutableSetMultimap; cdecl; overload;
  end;

  [JavaSignature('com/google/common/collect/ImmutableSetMultimap')]
  JImmutableSetMultimap = interface(JImmutableMultimap)
    ['{AA02AE05-8F74-4E67-ACC8-AB396AE590B8}']
//    function entries: JImmutableSet; cdecl; overload;
//    function entries: JSet; cdecl; overload;
//    function entries: JCollection; cdecl; overload;
//    function entries: JImmutableCollection; cdecl; overload;
//    function get(object_1: JObject): JImmutableCollection; cdecl; overload;
//    function get(object_1: JObject): JCollection; cdecl; overload;
//    function get(object_1: JObject): JSet; cdecl; overload;
//    function get(k: JObject): JImmutableSet; cdecl; overload;
//    function inverse: JImmutableMultimap; cdecl; overload;
//    function inverse: JImmutableSetMultimap; cdecl; overload;
//    function removeAll(object_1: JObject): JCollection; cdecl; overload;
//    function removeAll(object_1: JObject): JSet; cdecl; overload;
//    function removeAll(object_1: JObject): JImmutableCollection; cdecl; overload;
//    function removeAll(object_1: JObject): JImmutableSet; cdecl; overload;
//    function replaceValues(k: JObject; iterable: JIterable): JImmutableSet; cdecl; overload;
//    function replaceValues(object_1: JObject; iterable: JIterable): JSet; cdecl; overload;
//    function replaceValues(object_1: JObject; iterable: JIterable): JImmutableCollection; cdecl; overload;
//    function replaceValues(object_1: JObject; iterable: JIterable): JCollection; cdecl; overload;
  end;
  TJImmutableSetMultimap = class(TJavaGenericImport<JImmutableSetMultimapClass, JImmutableSetMultimap>) end;

implementation

end.
