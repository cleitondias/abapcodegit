*&---------------------------------------------------------------------*
*&  Include  zsd_reprocess_interface_cl
*&---------------------------------------------------------------------*

CLASS cl_handler_evt DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id es_row_no e_column_id,

      handle_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id  es_row_no,

      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING es_row_no e_column e_row,

      handle_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING e_dyndoc_id.

ENDCLASS.

CLASS cl_handler_evt IMPLEMENTATION.


  METHOD handle_toolbar.

    DATA:
            lst_toolbar TYPE stb_button.

    CLEAR lst_toolbar.
    lst_toolbar-function  = 'TRANSPORT'.
    lst_toolbar-icon      = icon_import_all_requests.
    lst_toolbar-quickinfo = text-t01.
    lst_toolbar-text      = text-t02.
    lst_toolbar-disabled  = abap_false.
    APPEND lst_toolbar TO e_object->mt_toolbar.

    IF p_ctt IS NOT INITIAL.
      CLEAR lst_toolbar.
      lst_toolbar-function  = 'SHIPMCLOSE'.
      lst_toolbar-icon      = icon_ws_ship.
      lst_toolbar-quickinfo = text-t03.
      lst_toolbar-text      = text-t04.
      lst_toolbar-disabled  = abap_false.
      APPEND lst_toolbar TO e_object->mt_toolbar.
    ENDIF.

  ENDMETHOD.

  METHOD handle_user_command.

    CREATE OBJECT w_interfaces.

    CASE e_ucomm.
      WHEN 'TRANSPORT'.
        PERFORM f_reprocess_interface.
        CALL METHOD w_alv->refresh_table_display.
      WHEN 'SHIPMCLOSE'.
        PERFORM f_close_shipment_ctt.
        CALL METHOD w_alv->refresh_table_display.
    ENDCASE.

  ENDMETHOD.

  METHOD handle_hotspot.

    READ TABLE t_saida INTO DATA(lst_saida) INDEX e_row_id-index.
    IF sy-subrc IS INITIAL.
      CASE e_column_id-fieldname.
        WHEN 'VBELN_VA'.
          IF NOT lst_saida-vbeln_va IS INITIAL.
            SET PARAMETER ID 'AUN' FIELD lst_saida-vbeln_va.
            CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'VBELN_VL'.
          IF NOT lst_saida-vbeln_vl IS INITIAL.
            SET PARAMETER ID 'VL' FIELD lst_saida-vbeln_vl.
            CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
          ENDIF.
      ENDCASE.
    ENDIF.

  ENDMETHOD.

  METHOD handle_button_click.


    DATA: lw_row       TYPE c LENGTH 10,
          lw_url(1000) TYPE c.
    lw_row = es_row_no-row_id.

    IF es_col_id-fieldname EQ |LNK_BT|.
      TRY.
          DATA(lst_saida) = t_saida[ lw_row ].
          IF sy-subrc IS INITIAL.
            IF lst_saida-interface EQ |003|.
              lw_url = lst_saida-link.
              IF NOT lw_url IS INITIAL.
                CALL FUNCTION 'CALL_BROWSER'
                  EXPORTING
                    url                    = lw_url
                    browser_type           = 'SAP Browser Viewer'
                    contextstring          = 'Business Browser'
                  EXCEPTIONS
                    frontend_not_supported = 1
                    frontend_error         = 2
                    prog_not_found         = 3
                    no_batch               = 4
                    unspecified_error      = 5
                    OTHERS                 = 6.
              ENDIF.
            ELSEIF lst_saida-interface EQ |002|.
***              PERFORM f_print_ctt_etiquetas USING lst_saida-link.
            ENDIF.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          CLEAR lst_saida.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD handle_double_click.

    DATA: lw_row       TYPE c LENGTH 10,
          lw_url(1000) TYPE c.
    lw_row = es_row_no-row_id.

    CASE sy-sysid.
      WHEN 'DRI' OR 'QRI'.
        DATA(lw_tnb) = |https://imoovit.tnb.pt/iMoovit.WebAccess/ClientContractTrackForm.aspx?StoreID=| && |999|
                        && |&ExternalRef=|.
      WHEN 'PRI'.
        lw_tnb = |https://imoovit.tnb.pt/iMoovit.WebAccess/ClientContractTrackForm.aspx?StoreID=| && |315|
                        && |&ExternalRef=|.
      WHEN OTHERS.
    ENDCASE.


    TRY.
        DATA(lst_saida) = t_saida[ lw_row ].
        IF sy-subrc IS INITIAL.
          IF e_column-fieldname EQ |TRACKING_NUM|.

            DATA(lw_link) = COND string( WHEN p_ctt IS NOT INITIAL THEN c_ctt
                                         WHEN p_chnr IS NOT INITIAL THEN c_chnr
                                         WHEN p_tnb IS NOT INITIAL THEN lw_tnb ).

            lw_url = lw_link && lst_saida-tracking_num.


            IF NOT lw_url IS INITIAL.
              CALL FUNCTION 'CALL_BROWSER'
                EXPORTING
                  url                    = lw_url
                  browser_type           = 'SAP Browser Viewer'
                  contextstring          = 'Business Browser'
                EXCEPTIONS
                  frontend_not_supported = 1
                  frontend_error         = 2
                  prog_not_found         = 3
                  no_batch               = 4
                  unspecified_error      = 5
                  OTHERS                 = 6.
            ENDIF.
          ELSEIF e_column-fieldname EQ |IDLOJA|.
            PERFORM f_popup_loja USING lst_saida.
          ENDIF.
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
        CLEAR lst_saida.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_top_of_page.
    PERFORM f_top_of_page USING dg_dyndoc_id.
  ENDMETHOD.

ENDCLASS.
