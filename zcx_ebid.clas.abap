class ZCX_EBID definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of GENERIC_ERROR,
      msgid type symsgid value 'ZEBID',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'EXCEPTION_TEXT',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of GENERIC_ERROR .
  data EXCEPTION_TEXT type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !EXCEPTION_TEXT type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_EBID IMPLEMENTATION.


  method CONSTRUCTOR ##ADT_SUPPRESS_GENERATION.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->EXCEPTION_TEXT = EXCEPTION_TEXT .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
