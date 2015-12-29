CLASS zcl_zebid_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zebid_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS searchasyoutypes_get_entityset
         REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ZEBID_DPC_EXT IMPLEMENTATION.


  METHOD searchasyoutypes_get_entityset.
**TRY.
*CALL METHOD SUPER->SEARCHASYOUTYPES_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
    DATA: lo_ebid TYPE REF TO zcl_ebid.

    CREATE OBJECT lo_ebid.
    READ TABLE it_filter_select_options
     ASSIGNING FIELD-SYMBOL(<fs_filter_so>)
     INDEX 1.
    IF <fs_filter_so> IS ASSIGNED.
      READ TABLE <fs_filter_so>-select_options
        ASSIGNING FIELD-SYMBOL(<fs_so>)
        INDEX 1.
      IF <fs_so> IS ASSIGNED.
         DATA(lt_result) = lo_ebid->search_as_you_type( <fs_so>-low ).
         loop at lt_result ASSIGNING FIELD-SYMBOL(<fs_result>).
          APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entity>).
          <fs_entity>-companyname = <fs_result>.
         ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.