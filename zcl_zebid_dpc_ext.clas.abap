class ZCL_ZEBID_DPC_EXT definition
  public
  inheriting from ZCL_ZEBID_DPC
  create public .

public section.
protected section.

  methods SEARCHASYOUTYPES_GET_ENTITYSET
    redefinition .
  methods SIMPLESEARCHSET_GET_ENTITYSET
    redefinition .
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


  METHOD simplesearchset_get_entityset.
    DATA: lo_ebid TYPE REF TO zcl_ebid.
    DATA: ls_search_req TYPE zebid_match_request.

    CREATE OBJECT lo_ebid.

    LOOP AT it_filter_select_options
     ASSIGNING FIELD-SYMBOL(<fs_filter_so>).
      READ TABLE <fs_filter_so>-select_options
        ASSIGNING FIELD-SYMBOL(<fs_so>) INDEX 1.
      CASE <fs_filter_so>-property.
        WHEN 'CompanyName'.
          ls_search_req-company_name = <fs_so>-low.
        WHEN 'City'.
          ls_search_req-city = <fs_so>-low.
      ENDCASE.
    ENDLOOP.

    lo_ebid->search(
      EXPORTING
        is_search_request  = ls_search_req
      IMPORTING
        rs_search_response = DATA(ls_search_res)
    ).

    LOOP AT ls_search_res-suggestion ASSIGNING FIELD-SYMBOL(<fs_suggestion>).
      APPEND <fs_suggestion>-company TO et_entityset.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
