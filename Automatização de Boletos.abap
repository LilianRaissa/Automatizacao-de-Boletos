*----------------------------------------------------------------------*
* Author.....: Lilian Raissa Gomes Silva                               *
* Date.......: 28/04/2023                                              *
* Description: Automatização de Boletos                                *
*----------------------------------------------------------------------*

REPORT z_teste.

*Tipos de tabela
TYPES:
* Dados contidos no arquivo
  BEGIN OF ty_arquivo,
    bukrs TYPE bseg-bukrs,    "Empresa
    belnr TYPE bseg-belnr,    "Documento
    gjahr TYPE bseg-gjahr,    "Ano
    fdtag TYPE bseg-fdtag,    "Data de vencimento
    brcde TYPE bseg-glo_ref1, "Linha digitável,
  END OF ty_arquivo,

  BEGIN OF ty_check,
    bukrs TYPE bseg-bukrs,       "Empresa
    belnr TYPE bseg-belnr,       "Documento
    gjahr TYPE bseg-gjahr,       "Ano
    buzei TYPE bseg-buzei,       "Item
    fdtag TYPE bseg-fdtag,       "Data de vencimento
    brcde TYPE bseg-glo_ref1, "Linha digitável
  END OF ty_check,

  BEGIN OF ty_alv,
    bukrs TYPE bseg-bukrs,    "Empresa
    belnr TYPE bseg-belnr,    "Documento
    gjahr TYPE bseg-gjahr,    "Ano
    fdtag TYPE bseg-fdtag,    "Data de vencimento
    brcde TYPE bseg-glo_ref1, "Linha digitável
    texto TYPE char255,       "Log de processamento
  END OF ty_alv.

*     Tabelas internas
DATA: gt_arquivo TYPE TABLE OF ty_arquivo,
      gt_check   TYPE TABLE OF ty_check,
      gt_doc     TYPE TABLE OF ty_check,
      gt_alv     TYPE TABLE OF ty_alv.

*    Estruturas
DATA: gwa_arquivo TYPE ty_arquivo,
      gwa_check   TYPE ty_check,
      gwa_doc     TYPE ty_check,
      gwa_alv     TYPE ty_alv.

*     Tabela para tratamento de arquivo
DATA gt_linha TYPE truxs_t_text_data.

**********************************************************************
*                          SELECTION SCREEN                          *
**********************************************************************
PARAMETERS: p_arq TYPE string.

**********************************************************************
*                         AT SELECTION SCREEN                        *
**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_arq.
  PERFORM f_seleciona_arq. "Abre caminho para selecionar o arquivo

**********************************************************************
*                         START OF SELECTION                         *
**********************************************************************
START-OF-SELECTION.

  PERFORM: f_upload_arq,  "Carrega o arquivo e deposita os dados em gt_linha
           f_trata_arq,   "Extrai os dados de gt_linha separadamente para a tabela gt_arquivo
           f_checa_dados. "Verifica se os dados de gt_arquivo existem na tabela BSEG

*--------------------------------------------------------------------*
*                        FORM f_seleciona_arq                        *
*--------------------------------------------------------------------*
FORM f_seleciona_arq.

  DATA: lt_file_table TYPE filetable.
  DATA: lv_rc TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title = 'Selecionar arquivo'
    CHANGING
      file_table   = lt_file_table
      rc           = lv_rc.

  IF sy-subrc IS INITIAL.
    READ TABLE lt_file_table ASSIGNING FIELD-SYMBOL(<fs_file_table>) INDEX 1.

    IF sy-subrc IS INITIAL.
      p_arq = <fs_file_table>-filename.
    ENDIF.
  ENDIF.

ENDFORM.

*--------------------------------------------------------------------*
*                         FORM f_upload_arq                          *
*--------------------------------------------------------------------*
FORM f_upload_arq.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename            = p_arq
      filetype            = 'ASC'
      has_field_separator = 'X'
    CHANGING
      data_tab            = gt_linha
    EXCEPTIONS
      OTHERS              = 19.

ENDFORM.

*--------------------------------------------------------------------*
*                          FORM f_trata_arq                          *
*--------------------------------------------------------------------*
FORM f_trata_arq.

  DATA: lwa_date TYPE c LENGTH 10,
        lwa_bukrs TYPE string,
        lwa_check TYPE i.

  LOOP AT gt_linha INTO DATA(ls_linha).

    SPLIT ls_linha AT ';' INTO: lwa_bukrs
                                gwa_arquivo-belnr
                                gwa_arquivo-gjahr
                                lwa_date
                                gwa_arquivo-brcde .

    CONCATENATE lwa_date+6(4) lwa_date+3(2) lwa_date(2) INTO gwa_arquivo-fdtag.
    lwa_check = strlen( lwa_bukrs ).

    IF lwa_check = 4. "Verifica se o campo possui número(Ex: 1000) ou texto(Ex: Empresa)
      gwa_arquivo-bukrs = lwa_bukrs.
      APPEND gwa_arquivo TO gt_arquivo.
    ENDIF.
  ENDLOOP.

ENDFORM.

*--------------------------------------------------------------------*
*                         FORM f_checa_dados                         *
*--------------------------------------------------------------------*
FORM f_checa_dados.

"Verifica se existem documentos na BSEG
  SELECT bukrs, belnr, gjahr, buzei, fdtag, glo_ref1
    FROM bseg
      INTO TABLE @gt_check
        FOR ALL ENTRIES IN @gt_arquivo
          WHERE bukrs = @gt_arquivo-bukrs
            AND belnr = @gt_arquivo-belnr
            AND gjahr = @gt_arquivo-gjahr
            AND koart = 'K'
            AND shkzg = 'H'.

  IF sy-subrc IS INITIAL.
    PERFORM: f_documentos,                "Verifica se o documento é parcelado ou não
             f_mapeamento,                "Realiza o mapeamento
             f_imprime_alv USING gt_alv.  "Cria arquivo externo
  ELSE.
    MESSAGE 'Dados não consistentes' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

*--------------------------------------------------------------------*
*                          FORM f_documentos                         *
*--------------------------------------------------------------------*
FORM f_documentos.

  SORT: gt_check   BY bukrs,
        gt_arquivo BY bukrs.

  LOOP AT gt_check INTO gwa_check.
    CLEAR gwa_doc.
    gwa_doc-buzei = gwa_check-buzei.

    LOOP AT gt_arquivo INTO gwa_arquivo WHERE belnr EQ gwa_check-belnr
                                          AND fdtag EQ gwa_check-fdtag.
      gwa_doc-bukrs = gwa_arquivo-bukrs.
      gwa_doc-belnr = gwa_arquivo-belnr.
      gwa_doc-gjahr = gwa_arquivo-gjahr.
      gwa_doc-fdtag = gwa_arquivo-fdtag.
      gwa_doc-brcde = gwa_arquivo-brcde.

      APPEND gwa_doc TO gt_doc.
      CLEAR: gwa_arquivo.

    ENDLOOP.

    CLEAR: gwa_check.
  ENDLOOP.

ENDFORM.
*--------------------------------------------------------------------*
*                          FORM f_mapeamento                         *
*--------------------------------------------------------------------*
FORM f_mapeamento.
  DATA lc_gname TYPE eqegraname VALUE 'BKPF'.

  DATA: lt_accchg TYPE TABLE OF accchg,
        lt_enq    TYPE TABLE OF seqg3.

  DATA: lv_texto TYPE char255,
        lv_garg  TYPE eqegraarg.

  CLEAR gwa_doc.
  LOOP AT gt_doc INTO gwa_doc.

    SELECT SINGLE glo_ref1 INTO @DATA(lv_glo_ref1_old)
      FROM bseg
        WHERE bukrs = @gwa_doc-bukrs
          AND belnr = @gwa_doc-belnr
          AND gjahr = @gwa_doc-gjahr.

    IF sy-subrc IS INITIAL.
      INSERT VALUE #( fdname = 'GLO_REF1'
                      oldval = lv_glo_ref1_old
                      newval = gwa_doc-brcde ) INTO TABLE lt_accchg.
    ENDIF.

    IF lt_accchg IS NOT INITIAL.

      CALL FUNCTION 'ENQUEUE_READ'
        EXPORTING
          gclient               = sy-mandt
          gname                 = lc_gname             " BKPF
          garg                  = lv_garg
        TABLES
          enq                   = lt_enq
        EXCEPTIONS
          communication_failure = 1
          system_failure        = 2
          OTHERS                = 3.

      IF sy-subrc EQ 0 AND lt_enq IS INITIAL.

        CALL FUNCTION 'FI_DOCUMENT_CHANGE'
          EXPORTING
            i_buzei              = gwa_doc-buzei
            i_bukrs              = gwa_doc-bukrs
            i_belnr              = gwa_doc-belnr
            i_gjahr              = gwa_doc-gjahr
          TABLES
            t_accchg             = lt_accchg
          EXCEPTIONS
            no_reference         = 1
            no_document          = 2
            many_documents       = 3
            wrong_input          = 4
            overwrite_creditcard = 5
            OTHERS               = 6.

        IF sy-subrc IS INITIAL.
          lv_texto = 'Processamento Efetuado com Sucesso'.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ELSE.

          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO DATA(lv_texto_ret).

          lv_texto = 'Processamento Não Efetuado' + lv_texto_ret.
        ENDIF.
      ELSE.
        lv_texto = 'Objeto com Enqueue Bloqueado'.
      ENDIF.
    ENDIF.

    PERFORM f_popula_alv USING lv_texto.

    CLEAR: lt_accchg,
           lv_texto.
  ENDLOOP.

ENDFORM.

*--------------------------------------------------------------------*
*                         FORM f_popula_alv                          *
*--------------------------------------------------------------------*
FORM f_popula_alv USING lv_texto_prc.
  gwa_alv-bukrs = gwa_doc-bukrs.
  gwa_alv-belnr = gwa_doc-belnr.
  gwa_alv-gjahr = gwa_doc-gjahr.
  gwa_alv-fdtag = gwa_doc-fdtag.
  gwa_alv-brcde = gwa_doc-brcde.
  gwa_alv-texto = lv_texto_prc.

  APPEND gwa_alv TO gt_alv.
  CLEAR  gwa_alv.

ENDFORM.

*--------------------------------------------------------------------*
*                         FORM f_imprime_alv                         *
*--------------------------------------------------------------------*
FORM f_imprime_alv USING gt_alv_display.

  DATA: lo_alv_table     TYPE REF TO cl_salv_table,
        lo_functions     TYPE REF TO cl_salv_functions,
        lo_columns_table TYPE REF TO cl_salv_columns_table,
        lo_column        TYPE REF TO cl_salv_column,
        lo_column_list   TYPE REF TO cl_salv_column_list.

  DATA: lv_list_display TYPE xfeld.

  TRY .
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          list_display = lv_list_display
        IMPORTING
          r_salv_table = lo_alv_table
        CHANGING
          t_table      = gt_alv_display.

      lo_alv_table->get_columns( )->set_optimize( 'X' ).
      lo_columns_table ?= lo_alv_table->get_columns( ).

      TRY.
          lo_column ?= lo_columns_table->get_column( columnname = 'FDTAG' ).
          lo_column->set_long_text( value = CONV scrtext_l( 'Data de Vencimento' ) ).
          lo_column->set_medium_text( value = CONV scrtext_m( 'Data de Vencimento' ) ).
          lo_column->set_short_text( value = CONV scrtext_s( 'Data de Vencimento' ) ).

          lo_column ?= lo_columns_table->get_column( columnname = 'BRCDE' ).
          lo_column->set_long_text( value = CONV scrtext_l( 'Linha Digitavel' ) ).
          lo_column->set_medium_text( value = CONV scrtext_m( 'Linha Digitavel' ) ).
          lo_column->set_short_text( value = CONV scrtext_s( 'Linha Digitavel' ) ).

          lo_column ?= lo_columns_table->get_column( columnname = 'TEXTO' ).
          lo_column->set_long_text( value = CONV scrtext_l( 'Log de Processamento' ) ).
          lo_column->set_medium_text( value = 'Log de Processamento' ).
          lo_column->set_short_text( value = CONV scrtext_s( 'Log de Processamento' ) ).

        CATCH cx_salv_not_found.
      ENDTRY.
      DATA(display) = lo_alv_table->get_display_settings( ).

      display->set_list_header( 'Dados Extraídos dos Boletos TUST' ).
      display->set_striped_pattern( cl_salv_display_settings=>true ).

      lo_alv_table->display( ).

    CATCH cx_salv_msg.
  ENDTRY.

ENDFORM.
