*******************************************************************************
*!* Primjer full join (ANSI)
*!* zamjena.dbf  popravak.dbf	REZULTAT
*!* --------------	--------------	---------------------------------
*!* rbr sifra opis	rbr sifra opis	rbr z_sifra z_opis p_sifra p_opis
*!* --------------	--------------	---------------------------------
*!* 1	z	 zam1	1	p	  pop1	1	z		zam1   p	   pop1
*!* 2	z	 zam2	2	m	  pop2	2	z		zam2   m	   pop2
*!* 3	p	 pop3	3					p	   pop3
*!* 4	m	 pop4	4					m	   pop4
*!*
*!* select z.rbr, z.sifra as z_sifra, z.opis as z_opis, p.sifra as p_sifra, p.opis as p_opis ;
*!*  from zamjena z, popravak p where z.rbr=p.rbr ;
*!*  union ;
*!*  select rbr, sifra as z_sifra, opis as z_opis, space(1) as p_sifra, space(10) as p_opis ;
*!*   from zamjena where rbr not in (select rbr from popravak) ;
*!*  union ;
*!*  select rbr, space(1) as z_sifra, space(10) as z_opis, sifra as p_sifra, opis as p_opis ;
*!*   from popravak where rbr not in (select rbr from zamjena) ;
*!*   order by 1
*******************************************************************************

*******************************************************************************
FUNCTION cpz
PARAMETERS polje
*
polje=STRTRAN(polje,'¨','»')
polje=STRTRAN(polje,'è','∆')
polje=STRTRAN(polje,'–','')
polje=STRTRAN(polje,'—','–')
polje=STRTRAN(polje,'¶','é')
polje=STRTRAN(polje,'Á','ö')
polje=STRTRAN(polje,'Ê','ä')
polje=STRTRAN(polje,'ü','Ë')
polje=STRTRAN(polje,'Ü','Ê')
polje=STRTRAN(polje,'ß','û')
polje=CONV_CASE(polje)
RETURN polje

*******************************************************************************
FUNCTION hr2us
PARAMETERS polje
*
polje=STRTRAN(polje,'Ë','c')
polje=STRTRAN(polje,'»','C')
polje=STRTRAN(polje,'Ê','c')
polje=STRTRAN(polje,'∆','C')
polje=STRTRAN(polje,'','dj')
polje=STRTRAN(polje,'–','Dj')
polje=STRTRAN(polje,'û','z')
polje=STRTRAN(polje,'é','Z')
polje=STRTRAN(polje,'ö','s')
polje=STRTRAN(polje,'ä','S')
* polje=CONV_CASE(polje)
RETURN polje


*!*	*******************************************************************************
*!*	FUNCTION convto1250
*!*	PARAMETERS pstr
*!*	pstr=STRTRAN(pstr, 'Á', 'ö')
*!*	pstr=STRTRAN(pstr, 'Ê', 'ä')
*!*	pstr=STRTRAN(pstr, '—', '–')
*!*	pstr=STRTRAN(pstr, '–', '')
*!*	pstr=STRTRAN(pstr, '¶', 'é')
*!*	pstr=STRTRAN(pstr, 'ß', 'û')
*!*	pstr=STRTRAN(pstr, '¨', '»')
*!*	pstr=STRTRAN(pstr, 'ü', 'Ë')
*!*	pstr=STRTRAN(pstr, 'è', '∆')
*!*	pstr=STRTRAN(pstr, 'Ü', 'Ê')
*!*	* 437 to 1250
*!*	pstr=STRTRAN(pstr, '{', 'ö')
*!*	pstr=STRTRAN(pstr, '[', 'ä')
*!*	pstr=STRTRAN(pstr, '\', '–')
*!*	pstr=STRTRAN(pstr, '|', '')
*!*	pstr=STRTRAN(pstr, '@', 'é')
*!*	pstr=STRTRAN(pstr, 'Ô', 'û')
*!*	pstr=STRTRAN(pstr, '^', '»')
*!*	pstr=STRTRAN(pstr, '~', 'Ë')
*!*	pstr=STRTRAN(pstr, ']', '∆')
*!*	pstr=STRTRAN(pstr, '}', 'Ê')
*!*	RETURN pstr

***************************************************************
* Draûen 27.06.2007
FUNCTION EMPTY_DATE
PARAMETERS PDATE
RETURN (EMPTY(PDATE) OR PDATE=QEMPTY_DATE)

*******************************************************************************
FUNCTION Razl_mjes && vraÊa razliku u mjesecima izmedu dva datuma
PARAMETERS PKasniji, PRaniji
*!*	RETURN (YEAR(PKasniji)-YEAR(PRaniji))*12+(MONTH(PKasniji)-MONTH(PRaniji))
RETURN qInsuranceObject.RazlikaMjeseciStrop((PKasniji), (PRaniji))
ENDFUNC && Razl_mjes

*******************************************************************************
FUNCTION Razlika_godina_strop && vraÊa razliku u godinama izmedu dva datuma
* napomena: strop znaËi da ako postoje prekobrojni dani zaokruûuje na cijelu godinu viöe
PARAMETERS PKasniji, PRaniji
PRIVATE XRet
IF EMPTY(PKasniji) OR PKasniji=Qempty_date
	RETURN 99
ELSE
	XRet=YEAR(PKasniji)-YEAR(PRaniji)
	IF GOMONTH(PRaniji, XRet*12) < PKasniji
		XRet=XRet+1
	ENDIF
ENDIF
RETURN XRet
ENDFUNC && Razlika_godina_strop

*******************************************************************************
FUNCTION TStr
PARAMETER PBroj,PDuzina,PDec
* 1. pretvori broj u string
* 2. odstranjuje praznine s lijeve strane
* 3. odstranjuje nepotrebne '0' s desne strane decimalnog zareza
* 4. odstranjuje decimalni zarez ako viöe nema decimala
PRIVATE XStr,XPoz
*
XStr=STR(PBroj,PDuzina,PDec)
IF PDec>0
	XPoz=PDuzina
	DO WHILE SUBSTR(XStr,XPoz,1)=='0'
		XPoz=Xpoz-1
	ENDDO
	IF INLIST(SUBSTR(XStr,XPoz,1),',','.')
		XPoz=Xpoz-1
	ENDIF
	XStr=LEFT(XStr,XPoz)
ENDIF
XStr=LTRIM(XStr)
IF EMPTY(XStr)
	XStr='0'
ENDIF
RETURN XStr
ENDFUNC && TStr

*******************************************************************************
PROCEDURE Wrap_txt && dijeli tekst na redove do odredene duûine
PARAMETERS PText,PWidth,PArr
* znaËenje parametara:
* PText je ulazni tekst - ne mijenja se unutar procedure
* PWidth je max. broj znakova u redu - ne mijenja se unutar procedure
* PArr je niz redova (rezultat) - mijenjaju se dimenzije i vrijednosti elemenata
PRIVATE GText,GLen,I
*
GText=ALLTRIM(PText)
I=1
DO WHILE LEN(GText)>PWidth
	GLen=RAT(' ',LEFT(GText,PWidth+1))
	IF GLen=0 && razmak nije naden, predugaËka rijeË - presjeci
		PArr[I]=LEFT(GText,PWidth)
		GText=SUBSTR(GText,PWidth+1)
	ELSE
		PArr[I]=LEFT(GText,GLen-1)
		GText=SUBSTR(GText,GLen+1)
	ENDIF
	I=I+1
	DIMENSION PArr[I]
ENDDO
PArr[I]=GText
RETURN
ENDPROC && Wrap_txt

*!*	***************************************************************
*!*	* vraÊa vrijednost iz tablice po uvjetu
*!*	* znaËenje parametara (ne mijenjaju se unutar procedure):
*!*	*	 PAtr  je ime polja (atributa) (SELECT) Ëija se vrijednost vraÊa
*!*	*	 PTabl je ime tablice (FROM)
*!*	*	 PUvjet je uvjet za traûenje sloga (WHERE)
*!*	*	 PDefault je vrijednost koja se vraÊa ako slog nije pronaen ili je NULL
*!*	***************************************************************
*!*	FUNCTION ret_xt1
*!*	PARAMETERS patr, ptabl, puvjet, pdefault, ptst
*!*	PRIVATE ssel, xret, atmp
*!*	DIMENSION atmp(1)
*!*	* ---
*!*	ssel = SELECT()
*!*	xret = pdefault
*!*	IF ml_sql([SELECT ]+patr+[ FROM ]+ptabl+ ;
*!*		IIF(!EMPTY(puvjet), [ WHERE ]+puvjet, []) + [ INTO array atmp])
*!*		IF _TALLY>0 AND !ISNULL(atmp(1))
*!*			xret = atmp(1)
*!*		ENDIF
*!*	ENDIF
*!*	SELECT (ssel)
*!*	RETURN xret
*!*	ENDFUNC && ret_xt1

*******************************************************************************
PROCEDURE NISTA
RETURN

***************************************************************
FUNCTION VALBP && vraÊa kontrolni broj po modulu 11 za bilo koju duûinu kljuËa
PARAMETERS PStrBroj
*!*	*
*!*	PRIVATE XStr,XLen,XSum,I
*!*	*
*!*	XStr=STRTRAN(STRTRAN(PStrBroj, ' ', ''), '-', '')
*!*	XLen=LEN(XStr)+2
*!*	XSum=0
*!*	FOR I=1 TO LEN(XStr)
*!*		XSum=XSum+VAL(SUBSTR(XStr,I,1))*(XLen-I)
*!*	ENDFOR
*!*	XSum=11-(XSum%11)
*!*	IF XSum>=10
*!*		XSum=0
*!*	ENDIF
*!*	RETURN LTRIM(STR(XSum))
RETURN qInsuranceObject.ValBP(PStrBroj)
ENDFUNC && VALBP

*******************************************************************************
FUNCTION ValMaticni && provjera ispravnosti JMBG-a
PARAMETERS pjmbg
*
IF valid_jmbg(pjmbg)
	RETURN .T.
ENDIF
abc_message('Upozorenje: uneöeni JMBG nema ispravan kontrolni broj!')
IF !qval_mat
	RETURN .T.
ENDIF
RETURN .F.
ENDFUNC && ValMaticni

*******************************************************************************
FUNCTION valid_jmbg		&& provjera ispravnosti JMBG-a
PARAMETERS pjmbg
*!*	PRIVATE xsum, i
*!*	*
*!*	IF LEN(ALLTRIM(pjmbg))=13
*!*		xsum = 0
*!*		FOR i = 1 TO 6
*!*			xsum = xsum + VAL(SUBSTR(pjmbg,i,1))*(8-i)
*!*		ENDFOR
*!*		FOR i = 7 TO 12
*!*			xsum = xsum + VAL(SUBSTR(pjmbg,i,1))*(14-i)
*!*		ENDFOR
*!*		xsum = 11 - (xsum % 11)
*!*		IF xsum > 9
*!*			xsum = 0
*!*		ENDIF
*!*		IF xsum = VAL(RIGHT(pjmbg,1))
*!*			RETURN .T.
*!*		ENDIF
*!*	ENDIF
*!*	RETURN .F.
RETURN qInsuranceObject.valid_jmbg(pjmbg)
ENDFUNC && valid_jmbg

*******************************************************************************
* Incubis d.o.o.   	              valid_oib           (c) All rights reserved *
* Author......: Robert					                     Date: 03.12.2009 *
* Description.: Funkcija za validaciju OIB-a                                  *
*******************************************************************************
FUNCTION valid_oib		&& provjera ispravnosti OIB-a
PARAMETERS poib
*!*	LOCAL xa, xi, xkontrolni, xk
*!*	IF LEN(poib) <> 11
*!*		RETURN .F.
*!*	ENDIF
*!*	xa = 10
*!*	FOR xi = 1 TO 10
*!*		xa = xa + VAL(SUBSTR(poib, xi, 1))
*!*		xa = MOD(xa, 10)
*!*		xa = IIF(xa==0, 10, xa)
*!*		xa = xa*2
*!*		xa = MOD(xa, 11)
*!*	ENDFOR
*!*	xkontrolni = 11 - xa
*!*	xkontrolni = IIF(xkontrolni = 10, 0, xkontrolni)
*!*	xk = VAL(SUBSTR(poib, 11, 1))
*!*	RETURN xk = xkontrolni
RETURN qInsuranceObject.valid_oib(poib)
ENDFUNC && valid_oib

*******************************************************************************
* Incubis d.o.o.   	              ret_mb_oib          (c) All rights reserved *
* Author......: Marko Bekafigo                               Date: 17.12.2009 *
* Description.: Funkcija vraÊa MB ili OIB poslovnog partnera                  *
*******************************************************************************
FUNCTION ret_mb_oib
PARAMETERS ppart, psto, plabel, pvalue, pmb, poib
PRIVATE ssel, xvalue, xret, xtip
ssel = SELECT()
*
IF PCOUNT()<4
	abc_message('POZIV:'+CHR(13)+CHR(13)+'ret_mb_oib(<cSifra>, <cSto>, <cLabel>, <cValue>)')
	RETURN .F.
ENDIF
IF PCOUNT()<5
	PRIVATE pmb, poib
	STORE '' TO pmb, poib
ENDIF
*
xtip=''
xret = .F.
xvalue=''
plabel=IIF(psto='P', ALLTRIM(qlbl_pol), ALLTRIM(qlbl_rac))
*
ml_sql("SELECT jmbg, mat_broj, pdv_br, sektor, domicil, tip FROM partneri WHERE jmbg = ?ppart INTO CURSOR tmppart")
IF _TALLY=1
	IF xfqmat_hok AND psto='P' AND VARTYPE(srpt_pol)='C' AND ALLTRIM(srpt_pol)=='POLHOK4'
		xvalue = pdv_br
	ELSE
		DO CASE
			CASE (psto = 'P' AND qpart_pol='1') OR (psto = 'F' AND qpart_rac='1')
				xvalue = jmbg
			CASE (psto = 'P' AND qpart_pol='2') OR (psto = 'F' AND qpart_rac='2')
				xvalue = mat_broj
			CASE (psto = 'P' AND qpart_pol='3') OR (psto = 'F' AND qpart_rac='3')
				xvalue = pdv_br
		ENDCASE
	ENDIF
	pvalue = ALLTRIM(xvalue)
	pmb = mat_broj
	poib = pdv_br
	plabel=IIF(EMPTY(poib),'',plabel)
	xtip=tip
	*
	* VraÊa .T. ako je sektor fiziËki i ne radi se kontrola oib-a ili je stranac ili je sve u redu
	xret = (sektor='1' AND qfakt_fiz_oib='OFF') OR domicil='I' OR (!EMPTY(xvalue))
	* Za Uniqu: ako se radi o grupnom partneru, vrati .T.
	xret=IIF(xfqmat_UNI AND chk_part_ino(xtip)=2, .T., xret)
ENDIF
*
USE IN tmppart
SELECT (ssel)
RETURN xret
ENDFUNC && ret_mb_oib

*******************************************************************************
PROCEDURE show_par_bez_oib
PRIVATE ssel
ssel=SELECT()
*
ml_sql([SELECT b.vip_status, a.ugo_id AS Sifra, b.naziv, a.datum, a.korisnik ]+;
[FROM par_boib a, partneri b ]+;
[WHERE a.ugo_id=b.jmbg AND b.pdv_br=' ' AND b.status<>'9' ]+;
[ORDER BY b.vip_status, ugo_id ]+;
[INTO CURSOR ccpboib ])
IF _tally>0
	PGC_OUT('ime_reporta',,'EXCEL')
ENDIF
USE IN ccpboib
*
SELECT(ssel)
*
ENDPROC

***************************************************************
PROCEDURE spremi_bez_oib
PARAMETERS pugo_id
PRIVATE ssel, apar_boib
DIMENSION apar_boib(1)
ssel=SELECT()
*
ml_sql([SELECT ugo_id ]+;
	[FROM par_boib ]+;
	[WHERE ugo_id=?pUgo_id ]+;
	[INTO ARRAY apar_boib])
IF _tally=0
	ml_sql([INSERT INTO par_boib (datum, ugo_id, korisnik ) VALUES (?DATE(), ?pugo_id, ?muser) ])
ENDIF
*
SELECT (ssel)
ENDPROC

*******************************************************************************
FUNCTION Valid_Ug && validacija ugovaratelja
PARAMETERS PSifra,PNaziv
* primjedba: osim vraÊanja primarnog rezultata (ispravnost +öifre ugovaratelja)
*	funkcija kao nuspojavu ("side effect") vraÊa i naziv ugovaratelja u 2. parametru
PRIVATE X1,X2,X3
STORE .F. TO X1,X2,X3
RETURN RetUgoParPol(PSifra,X1,PNaziv,X2,X3,.T.)
ENDFUNC && Valid_Ug

*******************************************************************************
* FUNCTION retugoparpol
*******************************************************************************
* Description: Traûi ugovaratelja/osiguranika u partnerima i policama
* Primjedba..: Osim vraÊanja primarnog rezultata (ispravnost+öifra ugovaratelja)
*              funkcija kao nuspojavu ("side effect") vraÊa tip, naziv, mjesto,
* 			   adresu i ev. dost.mjesto
*******************************************************************************
* Izmjene: 14.04.2014 - Pretraûivanje po OIB-u (Marko)
*******************************************************************************
FUNCTION retugoparpol
PARAMETERS psifra, ppartner, pnaziv, pmjesto, padresa, pext, pdost, pdostmj
PRIVATE sselret, xpostoji
sselret = SELECT()
*
IF PCOUNT()<8
	pdostmj = ''
ENDIF
IF PCOUNT()<7
	pdost = .F.
ENDIF
STORE .F. TO xpostoji, ppartner
psifra = PADR(LTRIM(psifra),13,' ')
*
* Prvi korak: traûimo u partnerima po OIB-u
IF LEN(ALLTRIM(psifra))=11 AND valid_oib(psifra)
	ml_sql([SELECT jmbg, pdv_br, sektor, naziv, mjesto, adresa, dost_naziv, dost_ptt, dost_adr, dost_mje, status ]+ ;
		[ FROM partneri WHERE pdv_br = ?psifra INTO CURSOR t_part1])
	IF _TALLY>0
		IF _TALLY=1
			psifra = jmbg
		ELSE
			ret = EXT(.F., psifra, 'psifra', 't_part1.ext_cursor', 'jmbg', , [STATUS<>'9'] + IIF(QSekt_part=1, [ AND sektor=']+ALLTRIM(GSektor)+ [' ], []))
		ENDIF
	ENDIF
ENDIF
*
* Nastavak: traûimo u partneriam po öifri
ml_sql([SELECT jmbg, pdv_br, sektor, naziv, mjesto, adresa, dost_naziv, dost_ptt, dost_adr, dost_mje, status ]+ ;
	[ FROM partneri WHERE jmbg = ?psifra INTO CURSOR t_part1])
IF _TALLY>0
	IF status = '9'
		abc_message('UPOZORENJE: '+CHR(13)+CHR(13)+'Status poslovnog partnera '+ALLTRIM(psifra)+' ('+;
			ALLTRIM(naziv)+') je "Neaktivan - Storniran"')
	ELSE
		XPostoji = .T.
		PPartner = (sektor=='2')
		IF PDost AND !EMPTY(dost_naziv)
			PNaziv = dost_naziv
			PMjesto = dost_ptt
			PAdresa = dost_adr
			PDostMj = ALLTRIM(dost_mje)
		ELSE
			PNaziv = naziv
			PMjesto = mjesto
			PAdresa = adresa	&& IIF(ALLTRIM(kbr)$adresa,adresa,ALLTRIM(adresa)+' ' +ALLTRIM(kbr))
			PDostMj = ''
		ENDIF
	ENDIF
ELSE
	* drugi korak: traûimo u policama
	IF (qsif_part=0) AND !qagent
		xpostoji=RetUgoIzPol(PSifra, PNaziv, PMjesto, PAdresa)
		IF xpostoji
			SELECT (SselRet)
			RETURN .T.
		ENDIF
	ENDIF
ENDIF
USE IN t_part1
SELECT (SselRet)
IF XPostoji
	RETURN .T.
ENDIF

* za AO ne koristi se Ext (to ne vaûi ako je qsif_part=1 - Obavezna kontrola unosa partnera)
IF (qsif_part=0) AND LEN(ALLTRIM(PSifra))>5 AND VARTYPE(PSto)='C' AND PSto='AO' AND !'.'$PSifra AND !ISALPHA(LEFT(PSifra,1))
   RETURN .T.
ENDIF

* treÊi korak: otvaramo ext u partnerima (ne koristi se za dost. adresu)
* ispituje se qsekt_part u EXT kao indikator moraju li se partneri filtrirati po sektoru s police (iz SYSSET-a) (Zrinko)
IF pext

	PRIVATE xnovi_part
	xnovi_part = ' '

	STORE '' TO extval, extval1, extval2
	extval3 = .F.
	xpostoji = ext(.F., psifra, 'psifra', 'partneri', 'jmbg', 'partner1', [status<>'9'] + IIF(qsekt_part=1, [ AND sektor=']+ALLTRIM(gsektor)+ [' ], []), 'naziv', , 'mjesto', , 'adresa', , 'sektor')

	* Marko (10.11.2017) - ako nije izabran partnera iz ext-a prebacivanje novog partnera na formu
	IF !xpostoji AND VARTYPE(xnovi_part)='C' AND !EMPTY(xnovi_part) AND xnovi_part<>'.'
		psifra = xnovi_part
	ENDIF

	ML_SQL('SELECT naziv, mjesto, adresa, sektor FROM partneri WHERE jmbg=?psifra INTO CURSOR tmpextpar')
	IF _TALLY > 0
		xpostoji = .T.
		extval = naziv
		extval1 = mjesto
		extval2 = adresa
		extval3 = sektor
	ENDIF
	USE IN tmpextpar
	IF XPostoji
		PPartner = (ExtVal3=='2')
		PNaziv	= ExtVal
		PMjesto = ExtVal1
		PAdresa = ExtVal2	&& IIF(ALLTRIM(ExtVal4)$ExtVal2,ExtVal2,ALLTRIM(ExtVal2)+' ' +ALLTRIM(ExtVal4))
	ENDIF
ENDIF
*
SELECT (SselRet)
RETURN XPostoji
ENDFUNC && RetUgoParPol

*******************************************************************************
FUNCTION RetUgoIzPol && traûi ugovaratelja u policama
PARAMETERS PSifra, PNaziv, PMjesto, PAdresa
* primjedba: osim vraÊanja primarnog rezultata (pronaden ugovaratelj)
*	funkcija kao nuspojavu ("side effect") vraÊa naziv, mjesto i adresu
PRIVATE Ssel, XPostoji, XTabl, XAPol, XAUgo
DIMENSION XAPol[1], XAUgo[3]
* ---
Ssel=SELECT()
XPostoji=.F.
IF RetNizPol('tab_pol', 'akt>0', 'XAPol')
	FOR EACH XTabl IN XAPol
		IF ML_SQL([SELECT ugo_naz, ugo_ptt, ugo_adr FROM ]+ALLTRIM(XTabl)+ ;
			[ WHERE ugo_id=']+PSifra+[' AND sektor<>'9' INTO ARRAY XAUgo])
			IF _TALLY>0
				XPostoji=.T.
				PNaziv =XAUgo[1]
				PMjesto=XAUgo[2]
				PAdresa=XAUgo[3]
			ENDIF
		ENDIF
		IF XPostoji
			EXIT
		ENDIF
	ENDFOR
ENDIF
SELECT (Ssel)
RETURN XPostoji
ENDFUNC && RetUgoIzPol

*******************************************************************************
PROCEDURE RetDostAdr && vraÊa dostavnu adresu ugovaratelja
PARAMETERS PPart, PPartNaz, PPartMje, PPartAdr, PPartDostMj
PRIVATE Ssel
Ssel=SELECT()
IF ML_SQL([SELECT naziv, adresa, mjesto, dost_naziv, dost_adr, dost_ptt, dost_mje ]+ ;
	[ FROM partneri ]+ ;
	[ WHERE jmbg=']+PPart+[' ]+ ;
	[ INTO CURSOR t_part1])
	IF _TALLY>0 AND !ISNULL(naziv) AND VARTYPE(naziv)=='C'
		IF !EMPTY(dost_naziv)
			PPartNaz=ALLTRIM(dost_naziv)
			PPartAdr=ALLTRIM(dost_adr)
			PPartMje=dost_ptt
			PPartDostMj=ALLTRIM(dost_mje)
		ELSE
			PPartNaz=ALLTRIM(naziv)
			PPartAdr=ALLTRIM(adresa)
			PPartMje=mjesto
			PPartDostMj=''
		ENDIF
	ENDIF
	USE IN t_part1
ENDIF
SELECT (Ssel)
RETURN
ENDPROC && RetDostAdr

*******************************************************************************
FUNCTION Ret1Par
PARAMETERS PVrsta,PFld
PRIVATE Ssel,GRbr,GDuz,GKlj,Ret
Ssel=SELECT()
*
Ret=''
STORE 0 TO GRbr,GDuz
DO CASE
	CASE TYPE(PFld)='N'
		Ret=0
	CASE TYPE(PFld)='C'
		Ret=''
	CASE TYPE(PFld)='D'
		Ret={}
	CASE TYPE(PFld)='L'
		Ret=.F.
ENDCASE
*
ML_SQL([SELECT * FROM vrst_os INTO CURSOR tmpvrstos])
GET_IND('tmpvrstos1','sifra')
*
GDuz=LEN(ALLTRIM(PVrsta))
FOR GRbr=GDuz TO 2 STEP -2
	GKlj=LEFT(PVrsta,GRbr)
	SEEK GKlj
	IF FOUND() AND !EMPTY(&PFld)
		Ret=&PFld
		GRbr=2
	ENDIF
NEXT
USE IN tmpvrstos
*
SELECT (Ssel)
RETURN Ret

*******************************************************************************
PROCEDURE MSG_NEMA
ABC_MESSAGE('Nema podataka koji zadovoljavaju uvjete!')
RETURN
ENDPROC && MSG_NEMA

*******************************************************************************
FUNCTION LAZI
ABC_MESSAGE('Nema podataka koji zadovoljavaju uvjete !')
RETURN

*******************************************************************************
FUNCTION Get_spec && vraÊa broj specifikacije vrijednosnih papira
PRIVATE Ssel, XBr_spec, XOk
* ---
Ssel=SELECT()
IF Inbase('SYSSET') AND DEF_DRV=='SQL'
	XOk=.F.
	ml_begintransaction()
	IF ML_SQL([SELECT br_spec FROM sysset WITH (UPDLOCK, SERIALIZABLE) INTO CURSOR t_br])
		IF _TALLY>0 AND !ISNULL(br_spec) AND VARTYPE(br_spec)=='N'
			XBr_spec=t_br.br_spec
			XOk=ML_SQL([UPDATE sysset SET br_spec=]+IIF(XBr_spec<99999999, [br_spec+1], [1]))
		ENDIF
		USE IN t_br
	ENDIF
	IF XOk
		ml_endtransaction() && commit
	ELSE
		ml_rollbacktransaction()
		XBr_spec=0
	ENDIF
ELSE
	XBr_spec=RET_XT1('br_spec', 'sysset', '1=1', 0)
	IF !ML_SQL([UPDATE sysset SET br_spec=]+IIF(XBr_spec<99999999, [br_spec+1], [1]))
		XBr_spec=0
	ENDIF
ENDIF
SELECT (Ssel)
RETURN PADL(LTRIM(STR(XBr_spec, 8, 0)), 8, '0')
ENDFUNC && Get_spec

*******************************************************************************
FUNCTION GET_RAC
RETURN BrFakt()
*!*	PRIVATE Ssel, XRet, XOk
*!*	* ---
*!*	Ssel=SELECT()
*!*	IF Inbase('SYSSET') AND DEF_DRV=='SQL'
*!*		XOk=.F.
*!*		ml_begintransaction()
*!*		IF ML_SQL([SELECT broj_rac FROM sysset WITH (UPDLOCK, SERIALIZABLE) INTO CURSOR t_br])
*!*			IF _TALLY>0 AND !ISNULL(broj_rac) AND VARTYPE(broj_rac)=='C'
*!*				XRet=t_br.broj_rac
*!*				XOk=ML_SQL([UPDATE sysset SET broj_rac=']+LEFT(XRet, 2)+ ;
*!*					PADL(LTRIM(STR(VAL(SUBSTR(XRet, 3))+1, 6, 0)), 6, '0')+['])
*!*			ENDIF
*!*			USE IN t_br
*!*		ENDIF
*!*		IF XOk
*!*			ml_endtransaction() && commit
*!*		ELSE
*!*			ml_rollbacktransaction()
*!*			XRet=SPACE(8)
*!*		ENDIF
*!*	ELSE
*!*		XRet=RET_XT1('broj_rac', 'sysset', '1=1', SPACE(8))
*!*		IF !EMPTY(XRet)
*!*			IF !ML_SQL([UPDATE sysset SET broj_rac=']+LEFT(XRet, 2)+ ;
*!*				PADL(LTRIM(STR(VAL(SUBSTR(XRet, 3))+1, 6, 0)), 6, '0')+['])
*!*				XRet=SPACE(8)
*!*			ENDIF
*!*		ENDIF
*!*	ENDIF
*!*	SELECT (Ssel)
*!*	RETURN XRet
ENDFUNC && GET_RAC


*******************************************************************************
PROCEDURE SET_RAC && uspostavi stanje takvo kao da je funkcija Get_rac kod zadnjeg poziva vratila PBroj_rac
PARAMETER PBroj_rac,Ssel
*!*	Ssel=SELECT()
*!*	ML_SQL([UPDATE sysset SET broj_rac=']+LEFT(PBroj_rac,2)+ ;
*!*		PADL(LTRIM(STR(VAL(SUBSTR(PBroj_rac,3))+1,6,0)),6,'0')+['])
*!*	SELECT (Ssel)
Abc_message('Nedozvoljeni postupak!' + CHR(13) + CHR(13) + ;
	'Prijavite problem nadleûnima: funkcija SET_RAC.')
Pgc_quit()
RETURN
ENDPROC && SET_RAC


*******************************************************************************
FUNCTION get_part
PRIVATE xret
xret = QCubisObject.get_part()
RETURN IIF(EMPTY(xret), SPACE(13), PADL(xret,13,'0'))
ENDFUNC && get_part

*!*	FUNCTION get_part
*!*	PRIVATE Ssel, XRet, XOk
*!*	* ---
*!*	Ssel=SELECT()
*!*	IF Inbase('SYSSET') AND DEF_DRV=='SQL'
*!*		XOk=.F.
*!*		ml_begintransaction()
*!*		IF ML_SQL([SELECT br_part FROM sysset WITH (UPDLOCK, SERIALIZABLE) INTO CURSOR t_br])
*!*			IF _TALLY>0 AND !ISNULL(br_part) AND VARTYPE(br_part)=='N'
*!*				XRet=t_br.br_part
*!*				XOk=ML_SQL([UPDATE sysset SET br_part=]+ALLTRIM(STR(XRet+1,13))+[])
*!*			ENDIF
*!*			USE IN t_br
*!*		ENDIF
*!*		IF XOk
*!*			ml_endtransaction() && commit
*!*		ELSE
*!*			ml_rollbacktransaction()
*!*			XRet=SPACE(13)
*!*		ENDIF
*!*	ELSE
*!*		XRet=RET_XT1('br_part', 'sysset', '1=1', SPACE(13))
*!*		IF !EMPTY(XRet)
*!*			IF !ML_SQL([UPDATE sysset SET br_part=']+ALLTRIM(STR(XRet+1,13))+[])
*!*				XRet=SPACE(13)
*!*			ENDIF
*!*		ENDIF
*!*	ENDIF
*!*	SELECT (Ssel)
*!*	RETURN IIF(EMPTY(XRet), SPACE(13), PADL(XRet,13,'0'))
*!*	ENDFUNC && GET_PART


*******************************************************************************
FUNCTION SET_K_F12()
ON KEY LABEL F12  &datafstring..OPIS1.SETFOCUS
RETURN

FUNCTION RESET_K_F12()
ON KEY LABEL F12
RETURN

FUNCTION SET_K_F12VAL()
ON KEY LABEL F12  &datafstring..OPIS2.SETFOCUS
RETURN

FUNCTION RESET_K_F12VAL()
ON KEY LABEL F12
RETURN

FUNCTION SETF12LP()
ON KEY LABEL F12  &datafstring..PREDMET1.SETFOCUS
RETURN

FUNCTION RESETF12LP()
ON KEY LABEL F12
RETURN

*******************************************************************************
FUNCTION Puni_Napom
PARAMETERS pbroj, pziro
PRIVATE gnap, gtxt1, xssel, xziro_nap, xiban_kv
* ---
xiban_kv = ''
IF xfqmat_kva
	IF qfaktnapo='NAPOMENA2'
		xssel =  SELECT()
		ml_sql([SELECT sifra, iban FROM ziro WHERE iban=?pziro OR sifra=?pziro OR ziro=?pziro INTO CURSOR ttempzir])
		IF _TALLY > 0
			xiban_kv = ALLTRIM(iban)
			xziro_nap = sifra
			xswift_kv = ' kod '  + alltrim(ret_ban_ziro(xziro_nap)) + ', swift '+Alltrim(ret_swift_ziro(xziro_nap))
		ELSE
			xswift_kv = ''
			xiban_kv = ''
		ENDIF
		USE IN ttempzir
		SELECT(xssel)
	ELSE
		xswift_kv = ' kod '  + alltrim(ret_ban_ziro(pziro)) + ', swift '+Alltrim(ret_swift_ziro(pziro))
	ENDIF
ELSE
	xswift_kv = ''
ENDIF
gnap = ''
gtxt1 = IIF(PCOUNT()=0, '', pbroj)
DO CASE
	CASE ALLTRIM(qfaktnapo)=='TABL'
		ml_sql([SELECT tekst FROM napomene WHERE proizvod=='FA' INTO CURSOR t_nap])
		gnap = IIF(_TALLY>0, ALLTRIM(tekst), '')
		USE IN t_nap
	CASE qfaktnapo='NAPOMENA1'
		gnap = 'Uplatu izvröite'+IIF(xfqmat_kva,'',' u roku od 8 dana')+' na ûiro raËun broj ' + ALLTRIM(ret_ext((pziro), 'ziro', , 'iban', 'sifra')) + xswift_kv + '.' + CHR(13) + ;
			'Molimo Vas da potvrdu o izvröenoj uplati dostavite na fax br. ' + ALLTRIM(qtlfx) + '.'
	CASE qfaktnapo='NAPOMENA2'
		*
		* Marko (30.06.2016) - Problem sa parametrom PZIRO - u napomenu staviti vrijednost parametra PZIRO - POTREBNA DETALJNIJA ANALIZA
		*
		gnap = 'Uplatu izvröite'+IIF(xfqmat_kva,'',' u roku od 8 dana')+' na ûiro raËun broj ' + IIF(PCOUNT()=2 AND !EMPTY(pziro) AND EMPTY(xiban_kv), ALLTRIM(pziro), IIF(!EMPTY(xiban_kv), ALLTRIM(xiban_kv), ALLTRIM(qiban))) + xswift_kv + '.' + CHR(13) + ;
			'Molimo Vas da potvrdu o izvröenoj uplati dostavite na fax br. ' + ALLTRIM(qtlfx) + '.'
		*
		*!*	GNap='Uplatu izvröite'+IIF(xfqmat_kva,'',' u roku od 8 dana')+' na ûiro raËun broj ' + ALLTRIM(Qiban) + xswift_kv + '.' + CHR(13) + ;
		*!*		'Molimo Vas da potvrdu o izvröenoj uplati dostavite na fax br. ' + ALLTRIM(QTLFX) + '.'
		*
	CASE qfaktnapo='NAPOMENA3'
		gnap = 'Za svrhu doznake upisati: Uplata dospjele premije po polici '+IIF(EMPTY(GTxt1),'(broj police)',ALLTRIM(GTxt1))+CHR(13)+;
			'Poziv na broj: 05 '+IIF(EMPTY(GTxt1),'(broj police)',ALLTRIM(GTxt1))
	CASE qfaktnapo='HER'
		gnap = "Molimo da prigodom plaÊanja po ovom dokumentu upiöete na nalog za plaÊanje u prostor "+CHR(13)+;
			"'Poziv na broj' : 05  "+IIF(EMPTY(GTxt1),'(broj raËuna)',ALLTRIM(GTxt1))
	CASE qfaktnapo='SUN'
		IF VARTYPE(sref_val)=='C' AND qosn_val<>sref_val
			gnap = 'Porez na dodanu vrijednost nije obraËunat - osloboeno plaÊanja poreza na dodanu vrijednost'+CHR(13)+;
				'temeljem Ël. 11. ToË. 2. Zakona o porezu na dodanu vrijednost.'+CHR(13)+CHR(13)+;
				'Iznos u kunama predstavlja protuvrijednost premije u '+ALLTRIM(SRef_val)+' prema srednjem teËaju HNB'+CHR(13)+;
				'na dan fakturiranja.'+CHR(13)+CHR(13)+;
				'Premija se plaÊa u protuvrijednosti kuna prema srednjem teËaju HNB na dan plaÊanja u'+CHR(13)+;
				'skladu s OpÊim uvjetima za osiguranje.'+CHR(13)+CHR(13)+;
				'RaËun je punovaljan bez ûiga i potpisa, jer je tiskan na elektroniËkom raËunalu.'
		ELSE
			gnap = 'Porez na dodanu vrijednost nije obraËunat - osloboeno plaÊanja poreza na dodanu vrijednost'+CHR(13)+;
				'temeljem Ël. 11. ToË. 2. Zakona o porezu na dodanu vrijednost.'+CHR(13)+CHR(13)+;
				'RaËun je punovaljan bez ûiga i potpisa, jer je tiskan na elektroniËkom raËunalu.'
		ENDIF
	OTHERWISE
		gnap = ''
ENDCASE
RETURN gnap
ENDFUNC && Puni_Napom

*******************************************************************************
FUNCTION GET_TEC && vraÊa teËaj
PARAMETERS PDatum,PValuta,PNaDan,PSel,PObrJed
PRIVATE SSel,SelTec,XNaDan,XDatum,XTecaj,XObrJed,XOnOff
* ---
SSel=SELECT()
XNaDan=IIF(PCOUNT()>=3 AND VARTYPE(PNaDan)=='L', PNaDan, .F.)  && PNaDan: ûelimo teËaj striktno na zadani datum
IF XNaDan
	XDatum=RET_XT1('tec_datum', 'tecaj', "tec_val='"+PValuta+"' AND tec_datum=?PDatum AND srednji>0", {})
ELSE
	XDatum=RET_XT1('MAX(tec_datum)', 'tecaj', "tec_val='"+PValuta+"' AND tec_datum<=?PDatum AND srednji>0", {})
ENDIF
IF XDatum={}
	XTecaj=0000000.000000
	DO WHILE XTecaj<=0
		XOnOff=SET('CURSOR')
		SET CURSOR ON
		XTecaj=PGC_RETVAL('TeËaj za '+PValuta+' na dan '+DTOC(PDatum),XTecaj)
		SET CURSOR &XOnOff
		IF XTecaj<=0
			ABC_MESSAGE('TeËaj mora biti veÊi od nule!')
		ENDIF
	ENDDO
	ML_SQL([INSERT INTO tecaj (tec_datum,tec_val,srednji) VALUES (?PDatum, ']+PValuta+[', ?XTecaj)])
ELSE
	XTecaj=RET_XT1('srednji', 'tecaj', "tec_val='"+PValuta+"' AND tec_datum=?XDatum", 1.000000)
ENDIF
* obraËunska jedinica
IF PCOUNT()>=5 AND !EMPTY(PObrJed)
	XObrJed=PObrJed
ELSE
	XObrJed=RET_XT1('obrjed', 'valute', "valuta_id='"+PValuta+"'", 1)
ENDIF
IF XObrJed#1
	XTecaj=ROUND(XTecaj/XObrJed,6)
ENDIF
SELECT (Ssel)
RETURN XTecaj
ENDFUNC && GET_TEC

*******************************************************************************
* VraÊa mjesto od PARTNERA u obliku "ptt_broj naziv_mjesta"
*******************************************************************************
FUNCTION RET_MJESTO
PARAMETER PSifra
*!*	PRIVATE XPtt_broj,XMjesto
*!*	XPtt_broj=RET_EXT((PSifra),'PARTNERI','PARTNER1','MJESTO')
*!*	XMjesto=RET_EXT((XPtt_broj),'MJESTA','MJESTA1','NAZIV')
*!*	RETURN XPtt_broj+' '+XMjesto
RETURN qInsuranceObject.ret_mjesto(PSifra)
ENDFUNC && RET_MJESTO

*******************************************************************************
PROCEDURE C_IZV(STO)
PRIVATE SSEL
SSEL=SELECT()
DO W_REKSTE WITH STO
SELECT (SSEL)
RETURN

*******************************************************************************
* IZNOS U SLOVIMA
*******************************************************************************
PROCEDURE slovima
	PARAMETERS pbroj, pval1, pval2
	PRIVATE xoblik, xslova, xmilijardi, xmiliona, xtisuca, xostatak, xlipa, xfull, xbroj
	IF VARTYPE(pval1)='C' AND pval1='EMP'
		STORE '' TO pval1, pval2
	ELSE
		IF EMPTY(pval1)
			pval1 = qosn_val
		ENDIF
		IF EMPTY(pval2)
			pval2 = '/100'
		ENDIF
	ENDIF
	*
	xbroj = pbroj
	xbroj = ROUND(xbroj,2)
	xfull = xbroj
	xlipa = (xbroj-INT(xbroj))*100
	xbroj = INT(xbroj)
	xostatak = xbroj%1000
	xbroj = INT(xbroj/1000)
	xtisuca = xbroj%1000
	xbroj = INT(xbroj/1000)
	xmiliona = xbroj%1000
	xbroj = INT(xbroj/1000)
	xmilijardi = xbroj%1000
	xslova = brojustr1(xmilijardi,xfull,.F.,4)+brojustr1(xmiliona,xfull,.F.,3)+;
		brojustr1(xtisuca,xfull,.F.,2)+brojustr1(xostatak,xfull,.T.,1)+;
		brojustr1(xlipa,xfull,.T.,0)
	RETURN LTRIM(xslova)
ENDPROC

*******************************************************************************
PROCEDURE brojustr1
	PARAMETERS xbroj, xfull, xzadnji, xgrupa
	PRIVATE xorig, xslova, xtmp
	
	xorig = xbroj
	xslova = ''
	xtmp = INT(xbroj / 100)*100
	
	IF xtmp > 0
		xslova = xslova + brojustr4(xtmp, xzadnji)
		xbroj = xbroj - xtmp
	ENDIF
	
	IF xbroj<20
		xslova = xslova + brojustr2(xbroj, xzadnji)
	ELSE
		xtmp = INT(xbroj / 10) * 10
		xslova = xslova + brojustr3(xtmp, xzadnji)
		xbroj = xbroj - xtmp
		IF xbroj>0
			xslova = xslova + brojustr2(xbroj, xzadnji)
		ENDIF
	ENDIF

	DO CASE
		CASE xgrupa=0
			IF xorig>0
				IF xfull>=1
					xslova = ' ' + pgc_dlang('i') + ' ' + xslova
				ENDIF
				DO CASE
					CASE INLIST(UPPER(pval1), 'KN', 'HRK')
						xslova = xslova + IIF(INLIST(xbroj,2,3,4), ' '+pgc_dlang('lipe'), ' '+pgc_dlang('lipa'))
					CASE UPPER(pval1)='EUR'
						DO CASE
							CASE xbroj=1
								xslova = xslova + ' ' + pgc_dlang('cent')
							CASE xbroj=1
								xslova = xslova + ' ' + pgc_dlang('cent')
							OTHERWISE
								xslova = xslova + ' ' + pgc_dlang('cent')
						ENDCASE
					OTHERWISE
						xslova = xslova + ' ' + pval2
				ENDCASE
			ENDIF
			
		CASE xgrupa=1
			IF xfull>=1
				DO CASE
					CASE INLIST(UPPER(pval1), 'KN', 'HRK')
						xslova = xslova + IIF(INLIST(xbroj,2,3,4), ' '+pgc_dlang('kune'), ' '+pgc_dlang('kuna'))
					CASE UPPER(pval1)='EUR'
					    DO CASE
							CASE xbroj=1
								xslova = xslova + ' '+pgc_dlang('Euro')
							OTHERWISE
								xslova = xslova + ' '+pgc_dlang('Eura')
						ENDCASE
					OTHERWISE
						xslova = xslova + ' ' + pval1
				ENDCASE
			ENDIF
			
		CASE xgrupa=2
		    IF xfqmat_bih			
				IF xorig=1
					xslova = pgc_dlang('hiljadu')
				ELSE
					IF xorig>1
						xslova = xslova + IIF(INLIST(xbroj,2,3,4), pgc_dlang('hiljade'), pgc_dlang('hiljada'))
					ENDIF
				ENDIF
			ELSE
				IF xorig=1
					xslova = pgc_dlang('tisuÊu')
				ELSE
					IF xorig>1
						xslova = xslova + IIF(INLIST(xbroj,2,3,4), pgc_dlang('tisuÊe'), pgc_dlang('tisuÊa'))
					ENDIF
				ENDIF
			ENDIF
			
		CASE xgrupa=3
		    IF xfqmat_bih			&& UPPER(PVAL1)='KN'
				IF xorig>0
					IF xorig=1
						xslova = pgc_dlang('milion')
					ELSE
						xslova = xslova + pgc_dlang('miliona')
					ENDIF
				ENDIF
			ELSE
				IF xorig>0
					IF xorig=1
						xslova = pgc_dlang('milijun')
					ELSE
						xslova = xslova + pgc_dlang('milijuna')
					ENDIF
				ENDIF
			ENDIF
			
		CASE xgrupa=4
			IF xorig=1
				xslova = pgc_dlang('milijardu')
			ELSE
				IF xorig>1
					xslova = xslova + IIF(INLIST(xbroj,2,3,4), pgc_dlang('milijarde'), pgc_dlang('milijardi'))
				ENDIF
			ENDIF
			
	ENDCASE
	RETURN xslova		&& +" "
ENDPROC

PROCEDURE brojustr2
	PARAMETERS xbroj, xzadnji
	PRIVATE xslova
	xslova = ""
	DO CASE
	CASE xbroj=1
		xslova = IIF(xzadnji, IIF(INLIST(UPPER(pval1), 'KN', 'KM'), "jedna", "jedan"), "jedna")
	CASE xbroj=2
		xslova = IIF(xzadnji, IIF(INLIST(UPPER(pval1), 'KN', 'KM'), "dvije", "dva"), "dvije")
	CASE xbroj=3
		xslova = "tri"
	CASE xbroj=4
		xslova = "Ëetiri"
	CASE xbroj=5
		xslova = "pet"
	CASE xbroj=6
		xslova = "öest"
	CASE xbroj=7
		xslova = "sedam"
	CASE xbroj=8
		xslova = "osam"
	CASE xbroj=9
		xslova = "devet"
	CASE xbroj=10
		xslova = "deset"
	CASE xbroj=11
		xslova = "jedanaest"
	CASE xbroj=12
		xslova = "dvanaest"
	CASE xbroj=13
		xslova = "trinaest"
	CASE xbroj=14
		xslova = "Ëetrnaest"
	CASE xbroj=15
		xslova = "petnaest"
	CASE xbroj=16
		xslova = "öesnaest"
	CASE xbroj=17
		xslova = "sedamnaest"
	CASE xbroj=18
		xslova = "osamnaest"
	CASE xbroj=19
		xslova = "devetnaest"
	ENDCASE
	RETURN pgc_dlang(xslova)
ENDPROC

PROCEDURE brojustr3
	PARAMETERS xbroj, xzadnji
	PRIVATE xslova
	xslova = ""
	DO CASE
	CASE xbroj = 20
		xslova = "dvadeset"
	CASE xbroj = 30
		xslova = "trideset"
	CASE xbroj = 40
		xslova = "Ëetrdeset"
	CASE xbroj = 50
		xslova = "pedeset"
	CASE xbroj = 60
		xslova = "öezdeset"
	CASE xbroj = 70
		xslova = "sedamdeset"
	CASE xbroj = 80
		xslova = "osamdeset"
	CASE xbroj = 90
		xslova = "devedeset"
	ENDCASE
	RETURN pgc_dlang(xslova)
ENDPROC

PROCEDURE brojustr4
	PARAMETERS xbroj, xzadnji
	PRIVATE xslova
	xslova = ""
	DO CASE
	CASE xbroj = 100
		xslova = "sto"
	CASE xbroj = 200
		xslova = "dvjesto"
	CASE xbroj = 300
		xslova = "tristo"
	CASE xbroj = 400
		xslova = "Ëetristo"
	CASE xbroj = 500
		xslova = "petsto"
	CASE xbroj = 600
		xslova = "öesto"
	CASE xbroj = 700
		xslova = "sedamsto"
	CASE xbroj = 800
		xslova = "osamsto"
	CASE xbroj = 900
		xslova = "devetsto"
	ENDCASE
	RETURN pgc_dlang(xslova)
ENDPROC

*******************************************************************************
FUNCTION Naplaceno && izracun uplacenog iznosa po fakturi
PARAMETERS PSelTmp
PRIVATE SSel,XRet,XSerial
* ---
XRet=0
SSel=SELECT()
SELECT (PSelTmp)
SCAN ALL
	XSerial=serial
	IF ML_SQL([SELECT SUM(n.iznos) AS uk_napl ]+ ;
		[ FROM naplate n, st_izvod q ]+ ;
		[ WHERE n.ser_izv=q.serial AND ]+ ;
			[ n.ser_rata=?XSerial AND q.id_trans IN ('NP','SN','PN') ]+ ;
		[ INTO CURSOR tmp_napl])
		XRet=XRet+uk_napl
		USE IN tmp_napl
	ENDIF
ENDSCAN
SELECT (Ssel)
RETURN XRet
ENDFUNC && Naplaceno

*******************************************************************************
FUNCTION ValAgeSura
IF QAgeSura AND (EMPTY(QAgencija) OR EMPTY(QSuradnik))
	ABC_MESSAGE('Nemate dodijeljenu suradniËku öifru. Molimo nazovite administratora sustava.')
	RETURN .F.
ENDIF
RETURN .T.

*******************************************************************************
PROCEDURE PRIB_OPS
PARAMETERS PSto,PFilter_vrste
PRIVATE Ssel,SelDok,xVar,GOk,XGetser
Ssel=SELECT()
*
* Izbjegavanje ESC Tipke
KEYBOARD CHR(254)
INKEY(0)
*
IF ML_SQL([SELECT * FROM ulazi_se WHERE 1=2 INTO CURSOR culazi_se])
	SelDok=SELECT()
	xVar=PGC_VAR()+',GNac_num,GDuzina_klj'
	PRIVATE &xVar
	STORE '' TO &xVar
	GDuzina_klj=13
	IF !EMPTY(IIF(PSto='POL',SPol_se,SPon_se))
		GOk=.T.
		DO WHILE GOk
			STORE '' TO &xVar
			GPribavljac=PADL(QSuradnik,6)
			GId_prod=QProd_mjesto
			GDokument=PADL(IIF(LEN(PFilter_vrste)=2,PFilter_vrste,'	'),2)
			GVrsta='1'
			GDatum=DATE()
			GOk=.F.
			GOd=PADR(GOd,13)
			GDo=PADR(GDo,13)
			GSlijedeci=PADR(Gslijedeci,13)
			Datafstring='PRIB_OPS'
			DO FORM PRIB_OPS NAME &Datafstring
			&Namestring..ENABLED=.T. && aktiviranje radnog stola
			PGC_CTRL()
			IF GOk
				XGetser=GET_SER()
				ML_SQL([INSERT INTO ulazi_se ]+ ;
					[(pribavljac, dokument, od, do, slijedeci, vrsta, datum, id_prod, serial, ind_ok) ]+ ;
					[VALUES (?GPribavljac, ?GDokument, ?GOd, ?GDo, ?GSlijedeci, ?GVrsta, ?GDatum, ?GId_prod ,?XGetser, 1) ])
			ENDIF
		ENDDO
	ELSE
		ABC_MESSAGE('Nije definiran dokument stroge evidencije!')
	ENDIF
	USE IN culazi_se
ENDIF
SELECT (Ssel)
RETURN

*******************************************************************************
PROCEDURE Iz_centar
DO Izv_cent
RETURN

*!*	*******************************************************************************
*!*	PROCEDURE RAZD_POL && raspored policirane premije
*!*	DO Pri_razd IN RAZD WITH .F.,.F.
*!*	RETURN
*!*	ENDPROC && RAZD_POL

*******************************************************************************
PROCEDURE RAZD_AGE && raspored policirane premije po agencijama (detalji u dat. razd_opc.prg)
DO r0 WITH 'A' IN RAZD_OPC
RETURN
ENDPROC && RAZD_AGE

*******************************************************************************
FUNCTION GO_TOP && za poziv prije showgrida
	GO TOP
ENDFUNC

*******************************************************************************
PROCEDURE OBAVJEST && Tisak obavjesti o isteku police, poziva OBAV_AGE ili OBAV_SVE
PARAMETERS PSifra
IF PCOUNT()=0
	PSifra='  '
ENDIF
DO &QFuncobav WITH PSifra	&& OBAV_AGE (lista agencija), OBAV_SVE (sve police bez agencija)
RETURN
ENDPROC && OBAVJEST

*******************************************************************************
PROCEDURE OBAV_SVE	&& Tisak obavijesti za sve police koje istiËu u periodu, bez agencija
PARAMETERS PSifra
DO OBAV_AGE WITH PSifra,.T. && .T. - dolazi iz OBAV_SVE
RETURN
ENDPROC && OBAV_SVE

*************************************************************************************************
* InCubis d.o.o.              OBAV_HIMO	            				   (C) All rights reserved
* Author......: Zrinko PuljiÊ		                        				  Date: 20.01.2014
* Description.: Obavijest o isteku polica HOK (jedan izvjestaj za sve police
*************************************************************************************************
PROCEDURE OBAV_HIMO
DO OBAV_AGE WITH 'IMO',.T.
ENDPROC

*************************************************************************************************
* InCubis d.o.o.              OBAV_AUTO	            				   (C) All rights reserved
* Author......: Zrinko PuljiÊ		                        				  Date: 05.01.2018
* Description.: Automatski tisak obavijesti o isteku za sve neûivotne proizvode (Uniqa)
*************************************************************************************************
PROCEDURE OBAV_AUTO
DO OBAV_AGE WITH 'AUT',.T.
ENDPROC

*******************************************************************************
PROCEDURE GR_PM && lista polica za obnovu grupirano po prodajnim mjestima (Sunce osig.)
PARAMETERS PGrup_pm
PGrup_pm='D'
DO OBAV_AGE WITH ' ',.T.,'P','*','4',1,0,PGrup_pm
RETURN
ENDPROC && GRUPNO_PM

*******************************************************************************
PROCEDURE OBAV_SUN	&& Tisak obavijesti za Sunce osiguranje d.d.
PARAMETERS PSifra
*
* Paramterti: Par1 = PSifra (oznaka proizvoda)
*			  Par2 = .T. (obavijesti za sve dospjele police ili samo za one koje imaju agencije u listi agencija)
*			  Par3 = 'P' (U gornjoj listi su agencije ili prodajna mjesta. Parametar joö sluûi i za filtriranje
*						  polica samo odreene agencije ili prodajnog mjesta za koje Êe se tiskati obavijesti)
*			  Par4 = QProd_mjesto (Ako je Par3='A' onda tu ide QAgencija, a ako je Par3='P' onda QProd_mjesto)
*			  Par5 = '3' (Izbor tiska '1'-Obavijesti, '2'-Lista, '3'-Obavijesti+Lista)
*			  Par6 = 1 (Dostavna adresa Lijevo/Desno   - 1=Lijevo, !1=Desno)
*			  Par7 = 0 (Samo tarifirane police ili sve - 1=Samo tarifirane, 0=Sve police)
*
IF EMPTY(PSifra) AND QModul='REMOTE'
	PSifra='10'
ENDIF
DO OBAV_AGE WITH PSifra,.T.,'P',(QProd_mjesto),'3',1,0
RETURN
ENDPROC && OBAV_SVE

*******************************************************************************
PROCEDURE SRED_TAP
PARAMETERS PId_pro
	DO P1 WITH PId_pro IN SRED_TAR
RETURN
ENDPROC && SRED_TAP

*******************************************************************************
PROCEDURE SRED_TAR1
* Marko 07.03.2009 - Funkcija tarifiranja po OS-u sa novom formom (sred_tar1) u kojoj se nalaze statusi tarifne kontrole
* Marko 10.10.2014 - Funkcija tarifiranja po OS-u sa formom sred_tar4
DO SRED_TAR WITH 'SRED_TAR4'
RETURN
ENDPROC && SRED_TAR1

*******************************************************************************
PROCEDURE SRED_TAR3
* Marko 10.11.2010 - Funkcija tarifiranja po OS-u sa novom formom (sred_tar3) u kojoj se nalaze statusi tarifne kontrole
* Marko 10.10.2014 - Funkcija tarifiranja po OS-u sa formom sred_tar4
DO SRED_TAR WITH 'SRED_TAR4'
RETURN
ENDPROC && SRED_TAR3

*******************************************************************************
PROCEDURE SRED_TAR4
DO SRED_TAR WITH 'SRED_TAR4'
RETURN
ENDPROC && SRED_TAR4

*******************************************************************************
PROCEDURE SRED_TAR5
DO SRED_TAR WITH 'SRED_TAR5'
RETURN
ENDPROC && SRED_TAR5

*******************************************************************************
PROCEDURE SRED_TAR6
DO SRED_TAR WITH 'SRED_TAR6'
RETURN
ENDPROC && SRED_TAR6

*******************************************************************************
PROCEDURE DOD_STR
*
Ukbr_str=Ukbr_str+1
IF Ukbr_str=9
	Ukbr_str=1
ENDIF
RETURN ''

*******************************************************************************
FUNCTION tekuca
PARAMETERS da,kojatab,dandanas
PRIVATE ssel
ssel=SELECT()
*
dandanas=DATE()
IF da
	IF kojatab='POZ'  && da li je promijenjena pozicija ili ne
		ML_SQL([INSERT INTO ag_poz (id_agent,id_poz,datum_poz) VALUES (?gid_sura,?gpoz_tekuca,?dandanas)])
	ELSE
		ML_SQL([INSERT INTO ag_nad (id_agent,id_nad,datum) VALUES (?gid_sura,?gid_nad,?gdat_nad)])
	ENDIF
ENDIF
*
SELECT (ssel)
RETURN .T.

*******************************************************************************
FUNCTION UGO_VRATI
IF !RetUgoParPol(GUgo_id,.F.,GUgo_naz,GUgo_ptt,GUgo_adr,!xfqmat_hok)
	STORE '' TO GUgo_naz
	IF (LEN(ALLTRIM(GUgo_id))<13 AND !QAdd_pra) OR (LEN(ALLTRIM(GUgo_id))=13 AND !ValMaticni(GUgo_id))
		&Datafstring..REFRESH
		RETURN .F.
	ENDIF
ENDIF
RETURN .T.

*******************************************************************************
FUNCTION Reg_str
PARAMETERS PReg_broj,PDatum,PAoAk
PRIVATE XRet
XRet=''
PAoAk=IIF(PCOUNT()<3,.T.,PAoAk)
IF LEN(ALLTRIM(PReg_broj))>2 AND PAoak
	XRet=XRet+' za vaöe vozilo registracijske oznake '+LEFT(PReg_broj,2)+' '+ALLTRIM(SUBSTR(PReg_broj,3))+' '
ENDIF
XRet=XRet+' istjeËe '+DTOC(PDatum)+'.'
RETURN XRet
ENDFUNC

*******************************************************************************
FUNCTION Reg_string
PARAMETERS PReg_broj,PDatum
PRIVATE XRet
XRet=''
IF LEN(ALLTRIM(PReg_broj))>2
	XRet=XRet+'registracijske oznake '+LEFT(PReg_broj,2)+ ;
		' '+ALLTRIM(SUBSTR(PReg_broj,3))+' '
ENDIF
XRet=XRet+'istjeËe '+DTOC(PDatum)+'.'
RETURN XRet
ENDFUNC

*******************************************************************************
FUNCTION Samos
PARAMETERS pagen
PRIVATE ssel,xret
ssel=SELECT()
*
xRet=.F.
ML_SQL([SELECT * FROM suradnci WHERE id_sura=?pagen INTO CURSOR tmpsursamos])
IF _TALLY>0 AND samos=1
	xret=.T.
ENDIF
USE IN tmpsursamos
*
SELECT (ssel)
RETURN xret
ENDFUNC

*******************************************************************************
FUNCTION dajprir   && vrati mi stopu prireza
PARAMETERS agent,ptt
PRIVATE ssel
ssel=select()
IF ret_ext(agent,'SURADNCI',,'OBRPOR','ID_SURA')=1
	GOpcina=ret_ext(ptt,'MJESTA','MJESTA1','OPCINA')
	PorDaNe=.T.
	SELECT(ssel)
	RETURN ret_ext(GOpcina,'OPCINE','OPCINE1','PRIREZ')
ENDIF
PorDaNe=.F.
SELECT(ssel)
RETURN 0.00

*******************************************************************************
FUNCTION Vrati_datum_prg  &&Vrati datum bildanja Megaline.exe
PARAMETERS TABLICA
	PRIVATE REZ,Xbr,ANiz
	REZ={}
	Xbr=ADIR(ANiz,UNX_HOM+TABLICA)
	IF XBR>0
		REZ=ANiz[1,3]
	ENDIF
	Xbr=ADIR(ANiz,TABLICA)
	IF XBR>0 AND ANiz[1,3]>REZ
		REZ=ANiz[1,3]
	ENDIF
RETURN REZ

*******************************************************************************
PROCEDURE Msg_Nije_fun
ABC_MESSAGE('Funkcija je trenutno joö u izradi.')
RETURN
ENDPROC && Msg_Nije_fun

*******************************************************************************
PROCEDURE List_PP1
DO Msg_Nije_fun
RETURN
ENDPROC && List_PP1

*******************************************************************************
PROCEDURE List_PP2
DO Msg_Nije_fun
RETURN
ENDPROC && List_PP2

*******************************************************************************
PROCEDURE List_st1
DO List_1 IN LIST_ST3 WITH 1
RETURN
ENDPROC && List_st1

*******************************************************************************
PROCEDURE List_st2
DO List_1 IN LIST_ST4 WITH 2
RETURN
ENDPROC && List_st2

*******************************************************************************
PROCEDURE List_st5
DO List_1 IN LIST_ST3 WITH 5
RETURN
ENDPROC && List_st5

*******************************************************************************
PROCEDURE List_st6
DO List_1 IN LIST_ST4 WITH 6
RETURN
ENDPROC && List_st6

*******************************************************************************
PROCEDURE Pregl_N1
DO Pregl_N WITH 'PREGL_1'
RETURN
ENDPROC && Pregl_N1

*******************************************************************************
PROCEDURE Pregl_N2
DO Pregl_N WITH 'PREGL_2'
RETURN
ENDPROC && Pregl_N2

*******************************************************************************
PROCEDURE Pregl_N3
DO Pregl_N WITH 'PREGL_3'
RETURN
ENDPROC && Pregl_N3

*******************************************************************************
PROCEDURE Pregl_N4
DO Pregl_N WITH 'PREGL_4'
RETURN
ENDPROC && Pregl_N4

*******************************************************************************
PROCEDURE Pregl_N5
DO Pregl_N WITH 'PREGL_5'
RETURN
ENDPROC && Pregl_N5

***************************************************************
* Naziv:	Sur_grupa
* Poziv:	DO Sur_grupa WITH (GId_sura),'tmp_sura' ili Sur_grupa(GId_sura,'tmp_sura')
* Namjena:	Lista suradnika sa svim podreenim suradnicima
* Opis:		Procedura popuni zadanu tablicu (drugi parametar) öiframa suradnika i
*			njihovim nivoima u grupi zadanog suradnika (prvi parametar);
*			struktura tablice je ista kao i struktura za "t_nivo" u proceduri;
*			struktura suradniËke grupe vadi se iz tablice "suradnci"
* Autor:	Goran
* Datum:	18.07.2003.
* Izmjene:	15.10.2005. struktura se vadi iz tabl. "suradnci" umjesto iz tabl. "sur_plus" uz dodatni uvjet "obr_neto=1" (nepoznati autor)
***************************************************************
PROCEDURE Sur_grupa
PARAMETERS PId_sura,PTabl
PRIVATE XPostoji,XNivo,XId_sura
* ---
CREATE CURSOR t_nivo (id_sura C(6), id_nad C(6), nivo N(3,0))
ZAP IN (PTabl)
ML_SQL([INSERT INTO ]+PTabl+[ (id_sura, id_nad, nivo) ]+ ;
	[ VALUES (']+PId_sura+[', ']+PId_sura+[', 0)])
XPostoji=.T.
XNivo=0
DO WHILE XPostoji AND XNivo<=100
	XPostoji=.F.
	ZAP IN t_nivo
	SELECT (PTabl)
	SCAN ALL FOR nivo=XNivo
		XId_sura=id_sura
		ML_SQL([SELECT id_sura, id_nad, ]+ALLTRIM(STR(XNivo+1,3,0))+[ AS nivo ]+ ;
			[ FROM suradnci ]+ ;
			[ WHERE obr_neto=1 AND id_sura<>']+XId_sura+[' AND id_nad=']+XId_sura+[' ]+ ;
			[ INTO CURSOR t_podr])
		IF _TALLY>0
			XPostoji=.T.
			SELECT t_nivo
			APPEND FROM DBF('t_podr')
		ENDIF
		USE IN t_podr
	ENDSCAN
	IF XPostoji
		SELECT (PTabl)
		APPEND FROM DBF('t_nivo')
	ENDIF
	XNivo=XNivo+1
ENDDO
USE IN t_nivo
RETURN
ENDPROC && Sur_grupa

***************************************************************
* Izbacio iz Agencije.prg jer ne radi niöta
***************************************************************
FUNCTION Age_Ugov
PARAMETERS PSifra,PNaziv
RETURN .T.
ENDFUNC

***************************************************************
* Izbacio iz Placanja.prg jer nisam pronaöao odakle se poziva
***************************************************************
FUNCTION tn_test
PARA Pdef
PRIVATE tester,Xprijenos_OK
ON ERROR XPRIJENOS_OK=.F.
Xprijenos_OK = .T.
tester = EVAL(Pdef)
ON ERROR PGC_ERROR(LINENO())
IF !Xprijenos_OK
	RETURN .F.
ENDIF
RETURN .T.

***************************************************************
* Izbacio iz Skup_os.prg jer nisam pronaöao odakle se poziva
***************************************************************
PROCEDURE RS_OBAV	&& Obavijesti o isteku polica
DO OBAVJEST WITH SIFRA
RETURN

***************************************************************
* Izbacio iz Partneri.prg jer nisam pronaöao odakle se poziva
***************************************************************
PROCEDURE TEH_REZ
PRIVATE xOk,Datafstring
xOk=.F.
IZJ_NUL('IZJ','G','*')
*
Datafstring='TEH_REZ'
DO FORM teh_rez NAME &Datafstring
*
IF xOk
	REPLACE bon_mal WITH GBon_mal,teh_rez WITH GTeh_rez
ENDIF
RETURN

***********************************************************************
* SHW_FORM - poziv defaultne forme sa 1 objektom (COMBO,EDIT,LIST,TEXT)
***********************************************************************
FUNCTION shw_form
PARAMETERS pok, pdatafstring, pcaption, pdescription, plabel, pobjtype, pctrlsrc, pctrlsrc2, plabel2, pctrlsrc3, plabel3
PRIVATE ssel, xvar2, xvar3
ssel = SELECT()
*
IF PCOUNT()<8
	xvar2 = ' '
	pctrlsrc2 = 'xvar2'
ENDIF
IF PCOUNT()<9
	plabel2 = '...'
ENDIF
IF PCOUNT()<10
	xvar3 = ' '
	pctrlsrc3 = 'xvar3'
ENDIF
IF PCOUNT()<11
	plabel3 = '...'
ENDIF
*
DO FORM shw_frm1 NAME &pdatafstring
*
SELECT (ssel)
RETURN .T.
ENDFUNC	&& shw_form

********************************************************************
* pp_cursor - generira ime temporary cursora
********************************************************************
PROCEDURE pp_cursor
RETURN 'W'+SUBSTR(SYS(2015),3)

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* Unix centar d.o.o.             UNOSSTRB           (C) All rights reserved *
* Author......: Kreöo                                      Date: 03.11.2003 *
* Description.: RS obrazac - moguÊnosti dodavanja i izmjene za str. B       *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
PROCEDURE UNOSSTRB
PRIVATE XOK
XOK=.T.
DO WHILE XOK
	UNOS_STR_B()
ENDDO
RETURN
ENDPROC

* * *
PROCEDURE UNOS_STR_B && Unos obustava djelatnika
PRIVATE F_List,F_Pict,F_Desc,F_Pred,F_Vali,F_Font,F_Width,;
	Dup_Izj,Dup_Rep,New_Row,Pre_Upd,Pos_Upd,Pre_Del,Pos_Del,;
	DefWhen,DefVali,DefFont,Can_APP,Brpo,BrMax,I,;
	Ssel,SelTmp,Tmp_file,Tmp_alias,SelTmp,XIdent
Ssel=SELECT()
*
XOK=.F.
IZJ_NUL('IZJ','G','*')
XIdent=PADR(ALLTRIM(GIdent),4)
*
ML_SQL([SELECT jmbg, prezime, opcina, osn_osig, vrs_inv, uve_staz, vrs_zdr, sati_rada,]+ ;
	[ obr_od, obr_do, bruttoa, bruttob, d_mioi, d_mioii, d_zdr, d_zap, izn_osig, osob_odb,]+ ;
	[ porez, prirez, netto, oib ]+ ;
	[ FROM RS_StrB ]+;
	[ WHERE ident=?Xident ]+;
	[ ORDER BY jmbg ]+;
	[ INTO CURSOR ccRS_StrB])
*
STORE 'PDA()' TO Dup_Izj,Dup_Rep,New_Row,Pre_Upd,Pos_Upd,Pre_Del,Pos_Del,DefWhen,DefVali
PRIVATE NulFunc,IzjFunc,GetFunc
NulFunc='PDA()'
IzjFunc='PDA()'
GetFunc='GetUnosStrB'
Pos_Upd='PosUpdStrB()'
New_Row='PuniStrB()'
*
Can_app=.T.
Deffont=8
Brpo=22
Brmax=22
DIMENSION F_LIST[BRPO],F_PICT[BRPO],F_DESC[BRPO],F_PRED[Brpo],F_VALI[BRPO],F_WIDTH[BRPO]
*
F_LIST[01]='JMBG'
F_LIST[02]='PREZIME'
F_LIST[03]='OPCINA'
F_LIST[04]='OSN_OSIG'
F_LIST[05]='VRS_INV'
F_LIST[06]='UVE_STAZ'
F_LIST[07]='VRS_ZDR'
F_LIST[08]='SATI_RADA'
F_LIST[09]='OBR_OD'
F_LIST[10]='OBR_DO'
F_LIST[11]='BRUTTOA'
F_LIST[12]='BRUTTOB'
F_LIST[13]='D_MIOI'
F_LIST[14]='D_MIOII'
F_LIST[15]='D_ZDR'
F_LIST[16]='D_ZAP'
F_LIST[17]='IZN_OSIG'
F_LIST[18]='OSOB_ODB'
F_LIST[19]='POREZ'
F_LIST[20]='PRIREZ'
F_LIST[21]='NETTO'
F_LIST[22]='OIB'
*
F_DESC[01]='JMBG'
F_DESC[02]='Djelatnik'
F_DESC[03]='äif. mj.'
F_DESC[04]='Osnova osig.'
F_DESC[05]='Vrsta inv.'
F_DESC[06]='UveÊani staû'
F_DESC[07]='Vrsta zdrav.'
F_DESC[08]='SATI'
F_DESC[09]='Od'
F_DESC[10]='do'
F_DESC[11]='Brutto pod 6.'
F_DESC[12]='Brutto pod 7.'
F_DESC[13]='Mir. I. stup'
F_DESC[14]='Mir. II. stup'
F_DESC[15]='Osn. zdrav.'
F_DESC[16]='Zapoöljavanje'
F_DESC[17]='Premije osig.'
F_DESC[18]='Osobni odbitak'
F_DESC[19]='Porez'
F_DESC[20]='Prirez'
F_DESC[21]='Netto'
F_LIST[22]='OIB'
*
F_PICT[01]=REPL('X',13)
F_PICT[02]=REPL('X',30)
F_PICT[03]=REPL('X',4)
F_PICT[04]=REPL('X',2)
F_PICT[05]='X'
F_PICT[06]='X'
F_PICT[07]='X'
F_PICT[08]=REPL('X',4)
F_PICT[09]=REPL('X',2)
F_PICT[10]=REPL('X',2)
F_PICT[11]='999999999.99'
F_PICT[12]='999999999.99'
F_PICT[13]='999999999.99'
F_PICT[14]='999999999.99'
F_PICT[15]='999999999.99'
F_PICT[16]='999999999.99'
F_PICT[17]='999999999.99'
F_PICT[18]='999999999.99'
F_PICT[19]='999999999.99'
F_PICT[20]='999999999.99'
F_PICT[21]='999999999.99'
F_PICT[22]=REPL('X',11)
*
F_Width[01]=12
F_Width[02]=24
F_Width[03]=7
F_Width[04]=9
F_Width[05]=9
F_Width[06]=9
F_Width[07]=9
F_Width[08]=6
F_Width[09]=4
F_Width[10]=4
F_Width[11]=10
F_Width[12]=10
F_Width[13]=10
F_Width[14]=10
F_Width[15]=10
F_Width[16]=10
F_Width[17]=10
F_Width[18]=10
F_Width[19]=10
F_Width[20]=10
F_Width[21]=10
F_Width[22]=12
*
F_Pred[01]='PDA()'
F_Pred[02]='PNE()'
F_Pred[03]='PDA()'
F_Pred[04]='PDA()'
F_Pred[05]='PDA()'
F_Pred[06]='PDA()'
F_Pred[07]='PDA()'
F_Pred[08]='PDA()'
F_Pred[09]='PDA()'
F_Pred[10]='PDA()'
F_Pred[11]='PDA()'
F_Pred[12]='PDA()'
F_Pred[13]='PDA()'
F_Pred[14]='PDA()'
F_Pred[15]='PDA()'
F_Pred[16]='PDA()'
F_Pred[17]='PDA()'
F_Pred[18]='PDA()'
F_Pred[19]='PDA()'
F_Pred[20]='PDA()'
F_Pred[21]='PDA()'
F_Pred[22]='PDA()'
*
SELECT (Ssel)
*
ML_SQL([SELECT jmbg, prezime, opcina, osn_osig, vrs_inv, uve_staz, vrs_zdr, sati_rada,]+ ;
	[ obr_od, obr_do, bruttoa, bruttob, d_mioi, d_mioii, d_zdr, d_zap, izn_osig, osob_odb,]+ ;
	[ porez, prirez, netto, oib ]+ ;
	[ FROM RS_StrB ]+;
	[ WHERE ident=?Xident ]+;
	[ ORDER BY jmbg ]+;
	[ INTO CURSOR ccStrB READWRITE])
*
SelTmp=SELECT()
Tmp_alias=ALIAS()
DO NOGRID WITH (SelTmp),'U','jmbg,prezime',,'Stavke str. B ','RS_STRB'
USE IN (SelTmp)
*
USE IN ccRS_StrB
SELECT (SSEL)
RETURN
ENDPROC

***************************************************************
PROCEDURE GETUNOSSTRB
PRIVATE Ssel,Xjmbg,Xprezime,Xopcina,Xosn_osig,Xvrs_inv,Xuve_staz,Xvrs_zdr, ;
	Xsati_rada,Xobr_od,Xobr_do,Xbruttoa,Xbruttob,Xd_mioi,Xd_mioii,Xd_zdr,Xd_zap, ;
	Xizn_osig,Xosob_odb,Xporez,Xprirez,Xnetto
STORE '' TO Xjmbg,Xprezime,Xopcina,Xosn_osig,Xvrs_inv,Xuve_staz,Xvrs_zdr,Xsati_rada, ;
	Xobr_od,Xobr_do
STORE 0 TO bruttoa,Xbruttob,Xd_mioi,Xd_mioii,Xd_zdr,Xd_zap,Xizn_osig,Xosob_odb,Xporez, ;
	Xprirez,Xnetto
Ssel=SELECT()
*
ON ERROR PGC_ERROR(LINENO())
*
SELECT ccRS_StrB
Xjmbg=jmbg
IF RECCOUNT()>0
	ML_SQL([DELETE FROM RS_StrB WHERE ident=?Xident AND jmbg=?Xjmbg])
	SELECT ccStrB
	SCAN ALL
		Xjmbg=jmbg
		Xprezime=prezime
		Xopcina=opcina
		Xosn_osig=PADL(ALLTRIM(osn_osig),2,'0')
		Xvrs_inv=vrs_inv
		Xuve_staz=uve_staz
		Xvrs_zdr=vrs_zdr
		Xsati_rada=PADL(ALLTRIM(sati_rada),4,'0')
		Xobr_od=PADL(ALLTRIM(obr_od),2,'0')
		Xobr_do=PADL(ALLTRIM(obr_dO),2,'0')
		Xbruttoa=bruttoa
		Xbruttob=bruttob
		Xd_mioi=d_mioi
		Xd_mioii=d_mioii
		Xd_zdr=d_zdr
		Xd_zap=d_zap
		Xizn_osig=izn_osig
		Xosob_odb=osob_odb
		Xporez=porez
		Xprirez=prirez
		Xnetto=netto
		xoib=oib
		*
		ML_SQL([INSERT INTO RS_STRB (ident, jmbg, prezime, opcina, osn_osig,]+ ;
				[ vrs_inv, uve_staz, vrs_zdr, sati_rada, obr_od, obr_do, bruttoa, bruttob,]+ ;
				[ d_mioi, d_mioii, d_zdr, d_zap, izn_osig, osob_odb, porez, prirez, netto, oib)]+ ;
			[ VALUES(?Xident, ?Xjmbg, ?Xprezime, ?Xopcina, ?Xosn_osig,]+ ;
				[ ?Xvrs_inv, ?Xuve_staz, ?Xvrs_zdr, ?Xsati_rada, ?Xobr_od, ?Xobr_do, ?Xbruttoa, ?Xbruttob,]+ ;
				[ ?Xd_mioi, ?Xd_mioii, ?Xd_zdr, ?Xd_zap, ?Xizn_osig, ?Xosob_odb, ?Xporez, ?Xprirez, ?Xnetto, ?xoib)])
	ENDSCAN
ENDIF
*
SELECT (Ssel)
RETURN
ENDPROC

***************************************************************
FUNCTION PosUpdStrB
*puniStrB()
DO CASE
	CASE 'jmbg'$E_Field
		REPLACE jmbg WITH Get_data1
	CASE 'prezime'$E_Field
		REPLACE prezime WITH Get_data1
	CASE 'oib'$E_Field
		REPLACE oib WITH Get_data1
ENDCASE
*
SELECT (tmp_alias)
GO TOP
*
RETURN

***************************************************************
FUNCTION puniStrB
PRIVATE Ssel,Xjmbg,Xprezime,Xopcina,Xosn_osig,Xvrs_inv,Xuve_staz,Xvrs_zdr, ;
	Xsati_rada,Xobr_od,Xobr_do,Xbruttoa,Xbruttob,Xd_mioi,Xd_mioii,Xd_zdr,Xd_zap, ;
	Xizn_osig,Xosob_odb,Xporez,Xprirez,Xnetto,Rez,xoib
STORE '' TO Xjmbg,Xprezime,Xopcina,Xosn_osig,Xvrs_inv,Xuve_staz,Xvrs_zdr,Xsati_rada, ;
	Xobr_od,Xobr_do,xoib
STORE 0.00 TO Xbruttoa,Xbruttob,Xd_mioi,Xd_mioii,Xd_zdr,Xd_zap,Xizn_osig,Xosob_odb,Xporez, ;
	Xprirez,Xnetto
Ssel=SELECT()
*
Xjmbg=Get_data1
ML_SQL([SELECT jmbg, prezime, opcina, osn_osig,]+ ;
	[ vrs_inv, uve_staz, vrs_zdr, sati_rada, obr_od, obr_do, bruttoa, bruttob,]+ ;
	[ d_mioi, d_mioii, d_zdr, d_zap, izn_osig, osob_odb, porez, prirez, netto, oib ]+ ;
	[ FROM ccRS_StrB]+ ;
	[ WHERE jmbg=?Xjmbg INTO CURSOR ccB])
IF _TALLY>0
	Xprezime=prezime
	Xopcina=opcina
	Xosn_osig=osn_osig
	Xvrs_inv=vrs_inv
	Xuve_staz=uve_staz
	Xvrs_zdr=vrs_zdr
	Xsati_rada='0000'
	Xobr_od=obr_od
	Xobr_do=obr_do
	xoib=oib
*!*		Xbruttoa=bruttoa
*!*		Xbruttob=bruttob
*!*		Xd_mioi=d_mioi
*!*		Xd_mioii=d_mioii
*!*		Xd_zdr=d_zdr
*!*		Xd_zap=d_zap
*!*		Xizn_osig=izn_osig
*!*		Xosob_odb=osob_odb
*!*		Xporez=porez
*!*		Xprirez=prirez
*!*		Xnetto=netto
	ML_SQL([INSERT INTO ccStrB (jmbg, prezime, opcina, osn_osig, vrs_inv, uve_staz,]+ ;
		[ vrs_zdr, sati_rada, obr_od, obr_do, bruttoa, bruttob,]+ ;
		[ d_mioi, d_mioii, d_zdr, d_zap, izn_osig, osob_odb,]+ ;
		[ porez, prirez, netto, oib)]+ ;
	[ VALUES(?Xjmbg, ?Xprezime, ?Xopcina, ?Xosn_osig, ?Xvrs_inv, ?Xuve_staz,]+ ;
		[ ?Xvrs_zdr, ?Xsati_rada, ?Xobr_od, ?Xobr_do, ?Xbruttoa, ?Xbruttob,]+ ;
		[ ?Xd_mioi, ?Xd_mioii, ?Xd_zdr, ?Xd_zap, ?Xizn_osig, ?Xosob_odb,]+ ;
		[ ?Xporez, ?Xprirez, ?Xnetto, ?xoib)])
ENDIF
USE IN ccB
*
SELECT (tmp_alias)
GO TOP
*
SELECT (Ssel)
RETURN
ENDPROC

***************************************************************
FUNCTION check_hub_rbr
PARAMETERS pdatum
PRIVATE ssel, xret, amaxi
DIMENSION amaxi[1]
STORE 0 TO amaxi[1]
STORE 1 TO xret
ssel=SELECT()
*
ml_sql([SELECT MAX(rbr) FROM virman WHERE datum=?pdatum INTO ARRAY amaxi])
xret=IIF(VARTYPE(amaxi[1])='N',amaxi[1]+1,1)
*
SELECT (ssel)
RETURN xret

******************************************************************************
* UNIX Centar d.o.o.              	PRM_PRST        (C) All rights reserved  *
* Author......: Andreas CrnkoviÊ                           	Date: 29.06.2004 *
* Description.: IzvjeöÊe o prometu po prodajnoj strukturi                    *
* Dorada      : Zamjena poziva funkcije PROM_PSX (doraen PROM_PS)-23.02.11. *
******************************************************************************
PROCEDURE prm_prst
*prom_ps('N')
prom_psx('N')
RETURN

******************************************************************************
* UNIX Centar d.o.o.              	PRS_PRST        (C) All rights reserved  *
* Author......: Andreas CrnkoviÊ                           	Date: 29.06.2004 *
* Description.: IzvjeöÊe o prometu po prodajnoj strukturi - NETO             *
* Dorada      : Zamjena poziva funkcije PROM_PSX (doraen PROM_PS)-23.02.11. *
******************************************************************************
PROCEDURE prs_prst
*prom_ps('S')
prom_psx('S')
RETURN

*******************************************************************************
* 28.10.2005. - PREMJEäTENO IZ FUNCPROV.PRG (Goran i Marko) - izbaËen "DO cp_tcpl" ili sliËno
* Dorada: Marko (10.10.2018) - u CC maila za Izvor dodana i adresa neispravnost.tarifa@izvorosiguranje.hr
*******************************************************************************
PROCEDURE oba_ndok
PRIVATE ssel, seltcpl, gpogreska, gpostupak, datafstring, gok, gnaz_age, gnaz_sura, gnaz_ugo, xugovor, ;
	xto, xcc, xbody, xnadlogin, xizlaz, xnadlogin_mail
ssel = SELECT()
*
xizlaz = 'A'
STORE SPACE(256) TO gpogreska, gpostupak
STORE ' ' TO gnaz_age, gnaz_sura, gnaz_ugo
*
izj_nul('IZJ','G','*')
*
xlogin = ALLTRIM(RET_EXT(gid_sura,'suradnci',,'login','id_sura'))
xvoditelj = ALLTRIM(RET_EXT(gid_sura,'suradnci',,'voditelj','id_sura'))
ml_sql([SELECT email FROM unx_user WHERE abcuser=?xlogin ])
IF _TALLY>0 AND !EMPTY(email)
	xto = ALLTRIM(email)
ELSE
	xto = ALLTRIM(RET_EXT(gid_sura,'suradnci',,'e_mail','id_sura'))
ENDIF
DO CASE
	CASE xfqmat_uni
		ml_sql([SELECT login FROM suradnci WHERE id_sura=?xvoditelj])
		IF _TALLY>0 AND !EMPTY(login)
			xlogin = login
			xcc = ALLTRIM(RET_EXT(xlogin,'unx_user',,'email','abcuser'))
		ENDIF
	CASE xfqmat_izv
		xnadlogin = ALLTRIM(RET_EXT(gid_sura,'suradnci',,'nadlogin','id_sura'))
		xnadlogin_mail = ALLTRIM(RET_EXT(xnadlogin,'suradnci',,'e_mail','login'))
		xcc = 'neispravnost.tarifa@izvorosiguranje.hr'+IIF(EMPTY(xnadlogin_mail), '', ';'+xnadlogin_mail)
	OTHERWISE
		xnadlogin = ALLTRIM(RET_EXT(gid_sura,'suradnci',,'nadlogin','id_sura'))
		xcc = ALLTRIM(RET_EXT(xnadlogin,'suradnci',,'e_mail','login'))
ENDCASE
gok = .F.
datafstring = 'OBA_NDOK'
SET CURSOR ON
DO FORM oba_ndok NAME &datafstring
SET CURSOR OFF
IF !gok
	SELECT (ssel)
	RETURN
ENDIF
ml_sql([SELECT COUNT(*) FROM sysset INTO CURSOR tmppolobav])
IF xizlaz='B'
	xxml=qReportsObject.ObavijestNeispravniDokument(gbroj, gpogreska, gpostupak)
	DO xml_out WITH xxml, 'OBA_NDOK'
ELSE
	DECLARE INTEGER ShellExecute ;
	IN SHELL32.DLL ;
	INTEGER nWinHandle,;
	STRING cOperation,;
	STRING cFileName,;
	STRING cParameters,;
	STRING cDirectory,;
	INTEGER nShowWindow
	ShellExecute(0, "", "mailto:"+ALLTRIM(xto)+IIF(EMPTY(ALLTRIM(xcc)),' ',"?CC="+ALLTRIM(xcc))+"&Subject=Neispravan dokument broj "+;
		ALLTRIM(gbroj)+"&Body="+ALLTRIM(gpogreska)+"%0A"+"%0A"+ALLTRIM(gpostupak), "", "",1)
	RETURN .T.
ENDIF
xxml = qReportsObject.ObavijestNeispravniDokument(gbroj, gpogreska, gpostupak)
DO xml_out WITH xxml, 'OBA_NDOK'
USE IN tmppolobav
*
SELECT (ssel)
RETURN

***************************************************************
PROCEDURE prem_akc && Promet po prodajnoj strukturi
	prem_ak(.T.,.T.) && po dat. izd, i neovisno o dat_ver [prem_ak.prg]
	RETURN

***************************************************************
* Marko (25.11.2015) - integracija polica AO u postojeÊi izvj.
***************************************************************
PROCEDURE prem_akao && Promet po prodajnoj strukturi
	prem_ak(.T.,.T.,.T.) && po dat. izd, i neovisno o dat_ver [prem_ak.prg] + AO
	RETURN

******************************************************************************
* UNIX Centar d.o.o.               tispol5           (C) All rights reserved *
* Author......: Marko Bekafigo                           	Date: 20.09.2006 *
* Description.: Otvara formu tispol5 i omoguÊava promjenu default. postavki  *
* Primjer.....: tispol5((gproizvod),(gid_grup),'tmptispol5')                 *
******************************************************************************
FUNCTION tispol5
	PARAMETERS pproizvod,pid_grup,pcursor
	PRIVATE ssel,xok,xvar
	ssel=SELECT()
	*
	ML_SQL([SELECT * FROM tis_pol5 WHERE id_grup=?pid_grup INTO CURSOR ]+pcursor+[ READWRITE])
	IF _TALLY=0
		ML_SQL([SELECT * FROM tis_pol5 WHERE proizvod=?pproizvod INTO CURSOR tmptis_pol5])
		ML_SQL([SELECT * FROM tmptis_pol5 WHERE (pid_grup="]+SPACE(8)+[" OR ]+;
			[LEFT(id_grup,LEN(ALLTRIM(id_grup)))==LEFT(?pid_grup,LEN(ALLTRIM(id_grup)))) ]+;
			[INTO CURSOR ]+pcursor+[ READWRITE])
		USE IN tmptis_pol5
		IF _TALLY=0
			SELECT (ssel)
			RETURN .F.
		ENDIF
	ENDIF
	*
	xok=.F.
	DO azur_tispol5 WITH pcursor IN tis_pol5
	*
	SELECT (ssel)
	RETURN xok

******************************************************************************
* UNIX Centar d.o.o.         ret_printer_tray        (C) All rights reserved *
* Author......: Marko Bekafigo                           	Date: 19.09.2006 *
* Description.: VraÊa printer i tray na koji ide tisak reporta (preport)     *
* Primjer.....: ret_printer_tray((xtispol5),'POL_ORIG',xprinter,xtray)       *
******************************************************************************
FUNCTION ret_printer_tray
	PARAMETERS pcursor,preport,pprinter,ptray
	PRIVATE ssel,xret,xrbr,xtmpprin,xtmptray,xtray
	ssel=SELECT()
	*
	xret=.T.
	SELECT &pcursor
	DO CASE
		CASE mrep1=preport
			xrbr='1'
		CASE mrep2=preport
			xrbr='2'
		CASE mrep3=preport
			xrbr='3'
		CASE mrep4=preport
			xrbr='4'
		CASE mrep5=preport
			xrbr='5'
		CASE mrep6=preport
			xrbr='6'
		CASE mrep7=preport
			xrbr='7'
		OTHERWISE
			xret=.F.
	ENDCASE
	IF xret
		xtmpprin='printer'+xrbr
		xtmptray='tray'+xrbr
		*
		pprinter=&xtmpprin
		xtray=&xtmptray
		*
		IF !ALLTRIM(pprinter)$'E K R A N,B E Z   P R I K A Z A'
			ML_SQL([SELECT * FROM prn_tray WHERE sifra=?xtray INTO CURSOR tmptray])
			IF _TALLY>0
				ptray=ALLTRIM(STR(oznaka))
			ELSE
				xret=.F.
			ENDIF
			USE IN tmptray
		ENDIF
	ENDIF
	*
	SELECT (ssel)
	RETURN xret

******************************************************************************
* UNIX Centar d.o.o.         tis_printer_tray        (C) All rights reserved *
* Author......: Marko Bekafigo                           	Date: 22.09.2006 *
* Description.: Tisak dokumenta na printer na tray i broj kopija             *
* Primjer.....: tis_printer_tray(xprinter,xtray,1)	                         *
******************************************************************************
FUNCTION tis_printer_tray
	PARAMETERS preport, pprinter, ptray, pbrkop
	DO CASE
		CASE ALLTRIM(pprinter)=='B E Z   P R I K A Z A'
			* Bez prikaza
		CASE ALLTRIM(pprinter)=='E K R A N'
			PGC_OUT(preport, , 'EKRAN', , , '', 0, .T., .T.)
		OTHERWISE
			PGC_OUT(preport, , 'PRINTER', pprinter, pbrkop, '', 0, .T., .T., ptray)
	ENDCASE
	RETURN .T.

******************************************************************************
* UNIX Centar d.o.o.              tis_obr3           (C) All rights reserved *
* Description.: IzbaËeno iz projekta                                         *
*******************************************************************************
PROCEDURE tis_obr3
	abc_message(pgc_dlang('Molimo Vas da se javite u In Cubis (+385 51 685407) ukoliko ste pozvali ovu funkciju')+' (TIS_OBR3)')
	RETURN

***************************************************************
* Naziv:	Razd_pol
* Poziv:	DO Razd_pol ili Razd_pol()
* Namjena:	Raspored policirane premije (po vrstama osiguranja)
* Autor:	Goran
* Datum:	<<14.01.2004.
* Izmjene:	03.03.2007. kod je ujednaËen i prebaËen u razd_sve.prg
***************************************************************
PROCEDURE Razd_pol
DO Razd_sve WITH 'P', 'Policiran'
RETURN
ENDPROC && Razd_pol

***************************************************************
* Naziv:	Razd
* Poziv:	DO Razd WITH .F. ili Razd(.F.) za dospjelu i
* 			DO Razd WITH .T. ili Razd(.T.) za naplaÊenu premiju
* Namjena:	Raspored dospjele i naplaÊene premije
* Autor:	Goran
* Datum:	<<14.01.2004.
* Izmjene:	03.03.2007. kod je ujednaËen i prebaËen u razd_sve.prg
***************************************************************
PROCEDURE RAZD
PARAMETERS PNapl
IF PNapl
	IF abc_upit('Y','éelite li analitiku po policama ?')
		DO Razd_sve WITH 'NA', 'NaplaÊen'
	ELSE
		DO Razd_sve WITH 'N', 'NaplaÊen'
	ENDIF
ELSE
		DO Razd_sve WITH 'D', 'Dospjel'

ENDIF
RETURN
ENDPROC && RAZD

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* Unix centar d.o.o.			  IME2PREZ	  (C) All rights reserved   	*
* Author......: Draûen									  Date: 19.06.2007  *
* Description.: Zamjena imena i prezimena									*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
FUNCTION IME2PREZ
	PARAMETERS pbroj, ptablica, pnepitati
	PRIVATE xrez,xbroj,ssel,xoboje
	ssel=SELECT()
	IF !EMPTY(pbroj)
		xbroj=pbroj
	ELSE
		xbroj=broj
	ENDIF
	IF !EMPTY(ptablica)
		xtablica=ptablica
	ELSE
		xtablica=stablica
	ENDIF
	ml_sql([SELECT broj,dat_ver,ugo_naz,osi_naz,ugo_id,osi_id FROM ]+xtablica+[ WHERE broj=?xbroj INTO CURSOR ccpolice1 READWRITE])
	xoboje=(osi_id==ugo_id)
	IF _tally>0
		IF !EMPTY(pnepitati) OR abc_upit('Y','éelite zamjeniti ime i prezime?')
			xprva=GETWORDNUM(ugo_naz,1)
			xrez=ALLTRIM(SUBSTR(ugo_naz,LEN(xprva)+1))+' '+xprva
			ml_sql([UPDATE ]+xtablica+[ SET ugo_naz=?xrez ]+IIF(xoboje,[, osi_naz=?xrez],[])+[ WHERE broj=?xbroj ])
			IF EMPTY(pnepitati)
				abc_message('Novi naziv ugovaratelja'+IIF(xoboje,[, osiguranika],[])+' '+xrez)
			ENDIF
		ENDIF
	ELSE
		abc_message('Ne postoji polica! '+xbroj)
	ENDIF
	USE IN ccpolice1
	SELECT (ssel)
RETURN .T.

***************************************************************
* Naziv:	B_TDK_sind
* Poziv:	DO B_TDK_sind ili B_TDK_sind()
* Namjena:	Tisak specifikacije trajnih naloga za sindikate na desni klik
* Opis:		()
* Autor:	Goran
* Datum:	07.05.2008.
***************************************************************
PROCEDURE B_TDK_sind
DO TS_SIND IN TN_SIND WITH (broj)
RETURN
ENDPROC && B_TDK_sind

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* In Cubis d.o.o.			pmt			   (C) All rights reserved  *
* Author......: Andreas CrnkoviÊ					  Date: 10.03.2009  *
* Description.: IzraËunava vrijednost uplata za zajam, koji se temelji na 		    *
* 		konstantnim uplatama i konstantnoj kamatnoj stopi.     			    *
*		pStopa	 - kamatna stopa za zajam					    *
*		pBr_rata - ukupan broj obroka u zajmu.					    *
*		pKredit  - sadaönja vrijednost, odnosno ukupan iznos sadaönje vrijednosti;  *
*			   takoer je poznata kao temeljna glavnica.			    *
*		pSaldo	 - buduÊa vrijednost ili saldo koji ûelite postiÊi nakon svih uplata*
*			   Ako je fv izostavljen, pretpostavlja se da je 0 (nula)	    *
*		pTip	 - broj 0 (nula) ili 1 i naznaËuje kada se obavljaju uplate.	    *
*			   0 ili je izostavljen - Ako je uplata obavljena na kraju razdoblja*
*			   1 			- Ako je obavljena na poËetku razdoblja     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
FUNCTION pmt
	PARAMETERS pStopa, pBr_rata, pKredit, pSaldo, pTip
	PRIVATE num,num2,x,xret
	IF pBr_rata==0.0
		RETURN 0
	ENDIF
	IF PCOUNT()<4
		pSaldo=0
		pTip=0
	ENDIF
	IF pstopa==0.0
		xret=(-pSaldo-pKredit)/pBr_rata
		RETURN ROUND(xret,2)
	ENDIF
	IF pTip!=0
		num=1+pStopa
	ELSE
		num=1
	ENDIF
	x=pStopa+1.0
	num2=x**pBr_rata
	xret=((-pSaldo-(pKredit*num2))/(num*(num2-1)))*pStopa

	RETURN ROUND(xret,2)
ENDFUNC


***************************************************************
FUNCTION pv5
	PARAMETERS pstopa, pbr_rata, pmt, psaldo, ptip
	RETURN qinsuranceobject.PV(pstopa, pbr_rata, pmt, psaldo, ptip)
ENDFUNC	&& pv5


***************************************************************
* Naziv:	prikaz_sp
* Autor:	Maja
* Opis:     Prikaz statusa nad ponudom
* Datum:	22.02.2011.
***************************************************************
PROCEDURE prikaz_sp
	prikaz_s('PON')
	RETURN
ENDPROC && prikaz_sp


***************************************************************
* Naziv:	prikaz_szk
* Autor:	Marko Bekafigo
* Opis:     Prikaz statusa nad zelenom kartom
* Datum:	08.02.2016
***************************************************************
PROCEDURE prikaz_szk
	prikaz_s('ZK')
	RETURN
ENDPROC && prikaz_szk


*****************************************************************************************************
PROCEDURE ext_part
	PARAMETERS psifra
	PRIVATE ssel, xpart, xpostoji, selpar, xok, xvar, getfunc, nulfunc, izjfunc, xzaklj, avrs_part, avipstatus, ;
		adrzave, apoziv_fix, apoziv_mob, xino, xold_datr, xtpoziv_drz, xtpoziv_ptt, xtpoziv_str, xtbroj, ;
		xmpoziv_drz, xmpoziv_ptt, xmpoziv_str, xmbroj, xnovi_part, xdigital, xid_digital
	ssel=SELECT()
	*
	xold_datr = {}
	STORE 0 TO xino, xdigital
	STORE xdrz_poziv TO xtpoziv_drz, xmpoziv_drz
	STORE '' TO xtpoziv_ptt, xtpoziv_str, xtbroj, xmpoziv_ptt, xmpoziv_str, xmbroj, xid_digital

	* Marko (10.11.2017) - varijabla za prebacivanje novog partnera na formu
	xnovi_part = ' '

	DIMENSION avrs_part[1], avipstatus[1], adrzave[1], apoziv_fix[1], apoziv_mob[1]

	fill_part_array()

	ml_sql([SELECT * FROM partneri WHERE jmbg=?psifra INTO CURSOR tmp_part READWRITE])
	selpar=SELECT()
	IF _TALLY = 1
		xpostoji = .T.
		xzaklj = (status='2')
		xok = .F.
		getfunc = 'getpartneri'
		izjfunc = 'izjpartneri'
		nulfunc = 'nulpartneri'
		*
		SELECT (selpar)
		*
		xvar=PGC_VAR()
		PRIVATE &xvar
		STORE '' TO &xvar
		*
		DO CASE
			CASE xfqmat_cen
				nogrid((selpar), IIF(xzaklj,'P','U'), 'jmbg, sektor', , 'partneri', IIF(xfqmat_bih, 'partneriy2rs', 'partneriy2'))
			CASE xfqmat_uni
				nogrid((selpar), IIF(xzaklj,'P','U'), 'jmbg, sektor', , 'partneri', 'partneriyun')
			OTHERWISE
				nogrid((selpar), IIF(xzaklj,'P','U'), 'jmbg, sektor', , 'partneri', IIF(xfqmat_bih, 'partneriyrs','partneriy'))
		ENDCASE
		*
		IF xok AND RECCOUNT()>0
			ml_append('tmp_part', 'partneri', 'jmbg=?gjmbg')
		ENDIF
	ELSE
		xpart = '.'

		* ext za izbor partnera - izabran se puni u varijabu xpart
		xpostoji = ext(.T., xpart, 'xpart', 'partneri','jmbg','partner1')

		* psifra (ugo_id ili osi_id) se mijenja u izabranog partnera (xpart) ako je varijabla xpostoji .t.
		psifra = IIF(xpostoji, xpart, psifra)

	ENDIF
	USE IN (selpar)
	SELECT (ssel)
	RETURN xpostoji

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* Unix centar d.o.o.              ret_ban_ziro      (C) All rights reserved *
* Author......: Robert                                     Date: 12.01.2010 *
* Description.: Funkcija vraÊa naziv banke kojoj pripada ûiroraËun ili      *
*               default banku                                               *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
FUNCTION ret_ban_ziro
PARAMETERS pid_ziro
PRIVATE xret
xret=qnaz_ban

IF xfqmat_hok OR xfqmat_jah OR xfqmat_kva
	xret = RET_XT1('naziv', 'banke', ;
		[sifra=']+RET_XT1('banka', 'ziro', [sifra=']+pId_ziro+['], ' ')+['], QNaz_ban)
ENDIF
RETURN xret

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* Unix centar d.o.o.                       (C) All rights reserved *
* Author......: Nikola                                     Date: 28.05.2013 *
* Description.: Funkcija vraÊa swift banke kojoj pripada ûiroraËun ili      *
*               default banku                                               *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
FUNCTION ret_swift_ziro
PARAMETERS pid_ziro
PRIVATE xret
xret=''

IF xfqmat_hok OR xfqmat_kva  && HOK ILI KVIG
	xret = RET_XT1('swift', 'banke', ;
		[sifra=']+RET_XT1('banka', 'ziro', [sifra=']+pId_ziro+['], ' ')+['], ' ')
ENDIF
RETURN xret

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* In Cubis d.o.o.              lis_vpap1            (C) All rights reserved *
* Author......: Zdravko & Marko                            Date: 26.01.2010 *
* Description.: Lista primljenih sredstava tipa C, K, X u periodu           *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
FUNCTION lis_vpap1
lis_vpap(1)
RETURN .T.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* In Cubis d.o.o.              lis_vpap2            (C) All rights reserved *
* Author......: Zdravko & Marko                            Date: 26.01.2010 *
* Description.: Lista primljenih sredstava tipa B u periodu                 *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
FUNCTION lis_vpap2
lis_vpap(2)
RETURN .T.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* In Cubis d.o.o.              lis_vpap             (C) All rights reserved *
* Author......: Zdravko & Marko                            Date: 26.01.2010 *
* Description.: Lista primljenih sredstava                                  *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
FUNCTION lis_vpap
PARAMETERS psto
PRIVATE ssel, xret, xsto, xdat1, xdat1, xxml AS String
ssel = SELECT()
*
xsto=''
DO CASE
	CASE psto=1
		xsto=' (C,K,X)'
	CASE psto=2
		xsto=' (B)'
ENDCASE
STORE DATE() TO xdat1, xdat2
*
xret = pgc_ret2val('Lista zaprimljenih sredstava'+xsto+' u periodu', 'Od datuma', 'Do datuma', 'xdat1', 'xdat2')
IF !xret
	SELECT (ssel)
	RETURN .T.
ENDIF
*
xxml = qReportsObject.ListaZapVrijedPapiraBonova_ver2((psto), "", DTOC(xdat1), DTOC(xdat2))
IF !EMPTY(qReportsObject.Error)
	IF (LEFT(qReportsObject.Error,3)=="OK!")
		XmlToRecordset(xxml)
		pgc_out(IIF(psto=1, 'BLA_PRE3_VER_2', 'BLA_PRE3'))
	ELSE
		abc_message("Greöka: "+qReportsObject.Error)
	ENDIF
ENDIF
*
SELECT (ssel)
RETURN .T.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* In Cubis d.o.o.              chk_part_ino         (C) All rights reserved *
* Author......: Marko Bekafigo                             Date: 16.02.2010 *
* Description.: Kontrola poslovnog partnera (stranca)                       *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
FUNCTION chk_part_ino && provjera da li je poslovni partner stranac
PARAMETERS ptip		&& , pdomicil
PRIVATE ssel, xret
ssel = SELECT()
*
xret=0
IF !EMPTY(ptip)	&& provjera stranca po atributu TIP
	ml_sql([SELECT ino FROM vrs_part WHERE sifra=?ptip INTO CURSOR tmpvrs_part])
	IF _TALLY=1
		xret=ino
	ENDIF
	USE IN tmpvrs_part
ENDIF
*
SELECT (ssel)
RETURN xret

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* In Cubis d.o.o.              chk_empty_part       (C) All rights reserved
* Author......: Marko Bekafigo                             Date: 16.02.2010
* Description.: Kontrola unosa min. jednog od slijedeÊi h podataka:
*             : matiËni broj, oib, broj osobne iskaznice, telefona
*			  : naziv, ime, prezime - Nikola 08.08.2011
*			  : datum ro, spol - Nikola 16.08.2011
* Izmjene.....: Marko (04.11.2011) - nema kontrole MAT_BROJ i PDV_BR za strance pravne osobe
*				Marko (14.12.2012) - dodana kontrola za adresu i mjesto partnera
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
FUNCTION chk_empty_part
PARAMETERS psektor, pino, pmat_broj, ppdv_br, pbr_osob, pbr_putov, ptelefon, pime, pprezime, pnaziv, pspol, pdatr, padresa, pmjesto
PRIVATE ssel, xret
ssel = SELECT()
*
xret = .T.
DO CASE
	CASE psektor='1'
		* Prvo provjera OIB-a ako je datum >= 01.01.2011
		IF qexist_oib AND (EMPTY(gpdv_br) OR !valid_oib(ALLTRIM(gpdv_br))) AND (VARTYPE(pino)='N' AND pino=0) AND DATE()>=DATE(2011,1,1)
			IF EMPTY(gpdv_br)
				abc_message(pgc_dlang('UPOZORENJE')+CHR(13)+CHR(13)+;
					pgc_dlang('Obavezan unos osobnog identifikacijskog broja (OIB)')+'!')
				xret = .F.
			ELSE
				IF LEN(ALLTRIM(gpdv_br))<>11
					abc_message(pgc_dlang('UPOZORENJE')+CHR(13)+CHR(13)+;
						pgc_dlang('Duljina osobnog identifikacijskog broja (OIB) mora biti 11 znakova')+'!')
					xret = .F.
				ELSE
					abc_message(pgc_dlang('UPOZORENJE')+CHR(13)+CHR(13)+;
						pgc_dlang('Uneöeni osobni identifikacijski broj (OIB) je neispravan')+'.')
					xret = .F.
				ENDIF
			ENDIF
		ELSE
			* Stara provjera kakva je vaûila do 31.12.2010.
			IF (pino=1 AND EMPTY(pbr_putov) AND EMPTY(pbr_osob)) OR (pino=0 AND EMPTY(pmat_broj) AND EMPTY(ppdv_br) AND EMPTY(pbr_osob))
				IF pino=1
					abc_message(pgc_dlang('Za fiziËke osobe (strance) obavezan je unos jednog od podataka')+':'+CHR(13)+CHR(13)+;
						'a) '+pgc_dlang('Broj putovnice')+CHR(13)+;
						'b) '+pgc_dlang('Broj osobne iskaznice'))
				ELSE
					abc_message(pgc_dlang('Za fiziËke osobe obavezan je unos barem jednog od podataka')+':'+CHR(13)+CHR(13)+;
						'a) '+pgc_dlang('MatiËni broj')+CHR(13)+;
						'b) '+pgc_dlang('OIB')+CHR(13)+;
						'c) '+pgc_dlang('Broj osobne iskaznice'))
				ENDIF
				xret = .F.
			ENDIF
		ENDIF
		IF xret AND qexist_spol AND VARTYPE(pspol) == 'C' AND EMPTY(pspol)
			abc_message(pgc_dlang('UPOZORENJE')+CHR(13)+CHR(13)+;
				pgc_dlang('Obavezan je unos spola')+'!')
			xret = .F.
		ENDIF
		IF xret AND qexist_datr AND (VARTYPE(pdatr) == 'D' OR VARTYPE(pdatr) == 'T') AND EMPTY(pdatr)
			abc_message(pgc_dlang('UPOZORENJE')+CHR(13)+CHR(13)+;
				pgc_dlang('Obavezan je unos datuma roenja')+'!')
			xret = .F.
		ENDIF
	CASE psektor='2'
		IF (VARTYPE(pino)='N' AND pino=0) AND EMPTY(pmat_broj) AND EMPTY(ppdv_br)
			abc_message(pgc_dlang('Za pravne osobe obavezan je unos')+':'+CHR(13)+CHR(13)+;
				'a) '+pgc_dlang('MatiËni broj')+CHR(13)+;
				'b) '+pgc_dlang('OIB'))
			xret = .F.
		ENDIF
ENDCASE
*
IF xret AND VARTYPE(pnaziv) == 'C' AND EMPTY(ALLTRIM(pnaziv))
	IF psektor='2'
		abc_message(pgc_dlang('UPOZORENJE')+CHR(13)+CHR(13)+;
			pgc_dlang('Obavezan je unos naziva partnera')+'!')
	ELSE
		abc_message(pgc_dlang('UPOZORENJE')+CHR(13)+CHR(13)+;
			pgc_dlang('Obavezan je unos imena i prezimena partnera')+'!')
	ENDIF
	xret = .F.
ENDIF
IF xret AND qexist_adr AND (EMPTY(padresa) OR (VARTYPE(xino)='N' and xino=0 AND EMPTY(pmjesto)))
	abc_message(pgc_dlang('UPOZORENJE')+CHR(13)+CHR(13)+;
		pgc_dlang('Obavezan unos adrese i mjesta partnera')+'!')
	xret = .F.
ENDIF
IF xret AND qexist_tel AND EMPTY(gtelefon)
	abc_message(pgc_dlang('UPOZORENJE')+CHR(13)+CHR(13)+;
		pgc_dlang('Obavezan unos kontakt telefona')+'!')
	xret = .F.
ENDIF
*
SELECT (ssel)
RETURN xret

***************************************************************
FUNCTION FRata && vraÊa broj rata u godini dana za poslanu dinamiku plaÊanja
PARAMETERS PDinamika
PRIVATE XRet
* ---
XRet=1
DO CASE
	CASE PDinamika='01' && mjeseËno
		XRet=12
	CASE PDinamika='02' && tromjeseËno
		XRet=4
	CASE PDinamika='03' && polugodiönje
		XRet=2
	CASE PDinamika='04' && godiönje
		XRet=1
ENDCASE
RETURN XRet
ENDFUNC && FRata


******************************************************
* Author......: Marko Bekafigo
* Datum.......: 27.04.2010
* Description.: Funkcija koja vraÊa vrijednost iz niza
******************************************************
PROCEDURE aretval
PARAMETERS parray6, pval, psrchcol, pcolumn, pdefval
PRIVATE xret, xpos
* Ako nije pronaen kljuË niza funkcija vraÊa pdefval, ako je proslijeen
* ili ovisno o tipu nultu ili praznu vrijednost
DO CASE
	CASE PCOUNT()=5
		xret = pdefval
	CASE VARTYPE(parray6(1, pcolumn)) = 'N'
		xret = 0
	CASE VARTYPE(parray6(1, pcolumn)) = 'D'
		xret = {}
	OTHERWISE
		xret = ''
ENDCASE
* Ako nije proslijeen parametar psrchcol, pretraûuje se prvi stupac
IF EMPTY(psrchcol)
	psrchcol = 1
ENDIF
* Ako nije proslijeen parametar pcolumn, vraÊa se vrijednost iz prvog stupca
IF EMPTY(pcolumn)
	pcolumn = 1
ENDIF
ON ERROR xpos = -1
xpos = ASCAN(parray6, pval, -1, -1, psrchcol, 15)
IF xpos > 0
	xret = parray6(xpos, pcolumn)
ENDIF
ON ERROR pgc_error(LINENO())
RETURN xret
ENDFUNC && aretval

********************************************************
* Author......: Marko Bekafigo
* Datum.......: 31.03.2010
* Description.: Funkcija koja vraÊa kuÊni broj iz adrese
********************************************************
FUNCTION ret_kbr
PARAMETERS padresa
PRIVATE ssel, xrbr, xadresa , xkbr, xadr
ssel = SELECT()
*
xadresa = IIF(PCOUNT()=1, padresa, adresa)
xadr = ALLTRIM(UPPER(xadresa))
xkbr = ' '
DO CASE
	CASE RIGHT(ALLTRIM(xadr),3)==' BB'
		xkbr = RIGHT(ALLTRIM(xadresa),2)
	CASE RIGHT(ALLTRIM(xadr),4)==' B.B'
		xkbr = RIGHT(ALLTRIM(xadresa),3)
	CASE RIGHT(ALLTRIM(xadr),5)==' B.B.'
		xkbr = RIGHT(ALLTRIM(xadresa),4)
	OTHERWISE
		kucni_broj((xadresa) , xkbr)
		IF EMPTY(xkbr) AND ','$xadresa AND RAT(',', ALLT(xadresa), 2)=0
			xadresa = ALLTRIM(LEFT(xadresa, RAT(',',ALLT(xadresa),1)-1))
			kucni_broj((xadresa) , xkbr)
		ENDIF
ENDCASE
RETURN xkbr

***************************************************************
PROCEDURE kucni_broj
PARAMETERS xadresa, xkuc_br
PRIVATE xbr, xadr, i, x
xkuc_br = ''
xbr = ALLT(SUBSTR(xadresa, RAT(' ', ALLT(xadresa), 1)))
xadr = ALLT(SUBSTR(xadresa, 1, RAT(' ', ALLT(xadresa), 1)))
IF ISALPHA(xbr) AND LEN(xbr) <= 2 AND !ISALPHA(ALLT(SUBSTR(xadresa, RAT(' ', ALLT(xadresa), 2))))
	xbr = ALLT(SUBSTR(xadresa, RAT(' ', ALLT(xadresa), 2)))
	xadr = ALLT(SUBSTR(xadresa, 1, RAT(' ', ALLT(xadresa), 2)))
ENDIF
IF !EMPTY(xbr)
	IF !ISALPHA(xbr) OR 'BB'$UPPER(xbr) OR '/'$UPPER(xbr) OR '-'$UPPER(xbr)
		xadresa = xadr
		xkuc_br = xbr
		x = 0
		FOR i = 1 TO LEN(RTRIM(xbr))
			ch = SUBSTR(xbr, i, 1)
			IF ISALPHA(ch) OR ch = ','
				x = x + 1
				IF x >= 3
					xadresa = xadresa + SUBSTR(xbr, x+1)
					xkuc_br = SUBSTR(xbr, 1, i-x)
					EXIT
				ENDIF
			ELSE
				x = 0
			ENDIF
		ENDFOR
	ENDIF
ENDIF
RETURN

***************************************************************
* Funkcija vraÊa da li aplikacijski matiËni broj (QMat_broj) pripada traûenoj oznaci
* Autor:	Goran
* Datum:	15.05.2010.
***************************************************************
FUNCTION FQMat_broj
PARAMETERS PNaziv
PRIVATE XRet, XNaziv, XMat_broj
XRet = .F.
XNaziv = UPPER(ALLTRIM(PNaziv))
XMat_broj = ALLTRIM(QMat_broj)
DO CASE
	CASE XNaziv == 'ALLIANZ'
		XRet = INLIST(XMat_broj, '3796795', '03796795')
	CASE XNaziv == 'ATLASLIFE'
		XRet = INLIST(XMat_broj, '02695227')
	CASE XNaziv == 'CAM'
		XRet = INLIST(XMat_broj, '2002927', '4263232820000')
	CASE INLIST(XNaziv, 'CO', 'CRO', 'CROSIG', 'CROOSIG', 'CROATIA')
		XRet = INLIST(XMat_broj, '3276147', '03276147')
	CASE INLIST(XNaziv, 'EH', 'EHZ', 'EHé')
		XRet = INLIST(XMat_broj, '1292463', '01292463', '0140660')
	CASE XNaziv == 'HO' && Hercegovina osig.
		XRet = INLIST(XMat_broj, '4227008550009')
	CASE XNaziv == 'HOK'
		XRet = INLIST(XMat_broj, '1578740', '01578740')
	CASE INLIST(XNaziv, 'JAH', 'JAHORINA')
		XRet = INLIST(XMat_broj, '1755927', '01755927')
	CASE XNaziv == 'KVIG'
		XRet = INLIST(XMat_broj, '1159941', '01159941', '1332872', '01332872')
	CASE XNaziv == 'LIDO'
		XRet = INLIST(XMat_broj, '4209330460009')
	CASE XNaziv == 'NAPREDAK'
		XRet = INLIST(XMat_broj, '2011142', '02011142')
	CASE XNaziv == 'SUNCE'
		XRet = INLIST(XMat_broj, '1220306', '01220306')
	CASE XNaziv == 'UNIQA'
		XRet = INLIST(XMat_broj, '1446452', '01446452')
	CASE XNaziv == 'IZVOR'
		XRet = INLIST(XMat_broj, '2688999', '02688999')
	CASE XNaziv == 'CENTRAL'
		XRet = INLIST(XMat_broj, '4202233240008')
	CASE XNaziv == 'TRIGLAV'
		XRet = INLIST(XMat_broj, '0253359')
ENDCASE
RETURN XRet
ENDFUNC && FQMat_broj

***************************************************************
* Funkcija vraÊa da li aplikacijski matiËni broj (QMat_broj) pripada traûenoj grupi oznaka
* Autor:	Goran
* Datum:	09.06.2010.
***************************************************************
FUNCTION FGrupa_mat_broj
PARAMETERS PGrupa
PRIVATE XGrupa
XGrupa = UPPER(ALLTRIM(PGrupa))
FOR i = 1 TO GETWORDCOUNT(XGrupa, ',')
	IF FQMat_broj(GETWORDNUM(XGrupa, i, ','))
		RETURN .T.
	ENDIF
ENDFOR
RETURN .F.
ENDFUNC && FGrupa_mat_broj

***************************************************************
* Funkcija: Akcija dodjela ovlasti generiranja OS samo do odreenog datuma za pojedine suradnike
* Autor:	Oli
* Datum:	07.06.2010.
* Parameters:
***************************************************************
FUNCTION OSDODAT
PARAMETERS DAILINE
PRIVATE SSEL
SSEL=SELECT()
GID_SURA=ID_SURA
IF QOSDODAT  && AKO POSTOJI POLJE U SURADNCIMA
	ML_SQL('SELECT OSDODAT FROM SURADNCI WHERE ID_SURA=?GID_sURA INTO CURSOR TMPSSS')
	IF  (DAILINE=1 AND OSDODAT=1) OR (DAILINE=0 AND OSDODAT=0)
		ABC_MESSAGE('Suradnik veÊ ima ovu ovlast. Slijedi povratak')
		USE IN TMPSSS
		SELECT(ssel)
		RETURN .F.
	ELSE
		ML_SQL('UPDATE SURADNCI SET OSDODAT=?DAILINE WHERE ID_SURA=?GID_SURA')
		IF dailine=1
			ABC_MESSAGE('Aktivacija uspjeöna!')
		ELSE
			ABC_MESSAGE('DeAktivacija uspjeöna!')
		endif
	ENDIF
	USE IN TMPSSS
ENDIF
SELECT(SSEL)
RETURN .T.

***************************************************************
* Funkcija: PASS_INFO_ADD(pabcuser, pabcime, pabcpass)
* Autor: Marko Bekafigo
* Datum: 14.07.2010.
* Opis : Tisak dokumenta sa pristupnim podacima (Lozinka.doc) obogaÊen
*        sa dodatnim podacima (npr. Uniqa je traûila suradniËku öifru)
***************************************************************
PROCEDURE pass_info_add
PARAMETERS pabcuser, pabcime, pabcpass
PRIVATE ssel
ssel = SELECT()
*
* Osnovni podaci
PRIVATE xuser, xpass, xime
xuser = ALLTRIM(pabcuser)
xpass = TRIM(abc_cr(pabcpass,pabcuser))
xime = ALLTRIM(pabcime)
*
* Dodatni podaci
PRIVATE xsura
xsura = ''
ml_sql([SELECT id_sura FROM suradnci WHERE login=']+PADR(pabcuser,10)+[' INTO CURSOR tmpinfor])
IF _TALLY>0
	xsura = id_sura
ENDIF
USE IN tmpinfor
*
* Tisak predloöka
WORD_OUT('Lozinka.doc')
*
SELECT (ssel)
RETURN .T.

***************************************************************
PROCEDURE LIST_PS1	&& Incubis funkcija - Poziv Wincubis funkcije LIST_PS (Marko - 21.03.2011)
DO LIST_PS
RETURN .T.
ENDFUNC && LIST_PS1

***************************************************************
PROCEDURE LIST_PS2	&& Incubis funkcija - Poziv Wincubis funkcije LIST_PS (Marko - 21.03.2011)
DO LIST_PS
RETURN .T.
ENDFUNC && LIST_PS2

***************************************************************
PROCEDURE LIST_PS3	&& Incubis funkcija - Poziv Wincubis funkcije LIST_PS (Marko - 21.03.2011)
DO LIST_PS
RETURN .T.
ENDFUNC && LIST_PS3

***************************************************************
PROCEDURE LIST_PS4	&& Incubis funkcija - Poziv Wincubis funkcije LIST_PS (Marko - 21.03.2011)
DO LIST_PS
RETURN .T.
ENDFUNC && LIST_PS4

***************************************************************
PROCEDURE SKADENC2	&& Incubis funkcija - Poziv Wincubis funkcije LIST_PS (Marko - 21.03.2011)
DO SKADENC1
RETURN .T.
ENDFUNC && SKADENC2

***************************************************************
PROCEDURE SKADENC3	&& Incubis funkcija - Poziv Wincubis funkcije LIST_PS (Marko - 21.03.2011)
DO SKADENC1
RETURN .T.
ENDFUNC && SKADENC3

***************************************************************
PROCEDURE SKADENC4	&& Incubis funkcija - Poziv Wincubis funkcije LIST_PS (Marko - 21.03.2011)
DO SKADENC1
RETURN .T.
ENDFUNC && SKADENC4

***************************************************************************
* Funkcija: kont_par_o()
* Autor: Marko Bekafigo
* Datum: 09.05.2011.
* Opis : Objedinjavanje viöe identiËnih poslovnih partnera sa jednom öifrom
***************************************************************************
PROCEDURE kont_par_o
PARAMETERS psifra, pcursor
PRIVATE ssel, xkont_par_o
ssel=SELECT()
*
IF PCOUNT()=0
	psifra = ' '
ENDIF
IF PCOUNT()<2
	pcursor = ' '		&& ALIAS()
ENDIF
*
xkont_par_o = 1
DO kont_par WITH psifra, pcursor
*
SELECT (ssel)
RETURN
ENDPROC && kont_par_o

***************************************************************************
* Funkcija: puni_errfile (opomene)
***************************************************************************
PROCEDURE puni_errfile
PARAMETERS pfile, ptext
IF PCOUNT()=0
	RETURN .F.
ENDIF
RETURN SpremiLog(pfile, TTOC(DATETIME())+CHR(13)+ptext)

***************************************************************************
* Funkcija: SpremiLog
***************************************************************************
PROCEDURE SpremiLog
PARAMETERS pfile, ptext
PRIVATE hlog
IF FILE(pfile)
	hlog = FOPEN(pfile, 12)
ELSE
	hlog=FCREATE(pfile)
ENDIF
FSEEK(hlog, 0, 2)
FWRITE(hlog, ptext+cr_lf)
FCLOSE(hlog)
RELEASE hlog
RETURN .T.
ENDPROC	&& SpremiLog

***************************************************************************
* Funkcija: ret_sif_part
* Autor: Marko Bekafigo 07.09.2011
* Opis: vraÊa oib ili mat_broj prema öifri partnera (parametar PSifra), a u ovisnosti od parametra PTips (O ili M)
***************************************************************************
PROCEDURE ret_sif_part
PARAMETERS psifra, ptips
PRIVATE ssel, ret
ssel = SELECT()
*
ret = psifra
ml_sql([SELECT jmbg, mat_broj, pdv_br FROM partneri WHERE jmbg=']+psifra+[' INTO CURSOR tmppart1])
IF _TALLY>0
	DO CASE
		CASE PTips='O'
			ret = pdv_br
		CASE PTips='M'
			ret = mat_broj
	ENDCASE
ENDIF
*
SELECT (ssel)
RETURN ret


***************************************************************************
* Funkcija: Izb_vrs_part
* Autor: Goran (prema funkciji "prik_par")
* Datum: 31.12.2012.
* Opis: popunjava parametre oznakom i nazivom tipa partnera i vraÊa uspjeönost
***************************************************************************
FUNCTION Izb_vrs_part
PARAMETERS psifra, pnaziv
PRIVATE ssel, ret

ssel = SELECT()
ret = .F.

IF ml_sql([SELECT 'Standardni partner' AS opis, '_' AS sifra FROM sysset ] + ;
	[ UNION ] + ;
	[ SELECT opis, sifra FROM vrs_part ] + ;
	[ ORDER BY sifra ] + ;
	[ INTO CURSOR t_vrs_part])

	ret = EXT(.F., psifra, 'psifra', 't_vrs_part.ext_cursor', 'sifra', , , 'opis')
	IF ret
		pnaziv = extval
		IF psifra = '_'
			psifra = ' '
		ENDIF
	ENDIF

	USE IN t_vrs_part
ENDIF

SELECT (ssel)
RETURN ret
ENDFUNC && Izb_vrs_part


*******************************************************************************
* Incubis d.o.o.   	              izb_mb_oib          (c) All rights reserved *
* Author......: Marko Bekafigo                               Date: 13.09.2011 *
* Description.: Izbor partnera (vraÊa MB ili OIB poslovnog partnera)          *
*******************************************************************************
FUNCTION izb_mb_oib
PARAMETERS ppart, ppartnaz, psto
PRIVATE ssel, xret
ssel = SELECT()
*
xret = .F.
ml_sql([SELECT jmbg, mat_broj, pdv_br, naziv ]+;
	[ FROM partneri ]+;
	[ WHERE ]+IIF(psto='M', [ mat_broj ], [ pdv_br ])+[ LIKE ']+ALLTRIM(ppart)+[%' ]+;
	[ INTO CURSOR ccpart_mb_oib])
DO CASE
	CASE _TALLY=1
		ppart = IIF(psto='M', mat_broj, pdv_br)
		ppartnaz = naziv
		xret = .T.
	CASE _TALLY>0
		IF ext(.T., ppart, 'ppart', 'ccpart_mb_oib.ext_ccpart_mb_oib', IIF(psto='M', 'mat_broj', 'pdv_br'))
			ppartnaz = RET_XT1('naziv', 'partneri', IIF(psto='M', [mat_broj='], [pdv_br='])+ppart+['], '')
		ENDIF
		xret = .T.
ENDCASE
USE IN ccpart_mb_oib
*
SELECT (ssel)
RETURN xret
ENDFUNC && izb_mb_oib

***************************************************************
PROCEDURE ext_ccpart_mb_oib
	PARAMETERS unifil, povname
	ext_cur(unifil, povname, 'ccpart_mb_oib')
	RETURN
ENDFUNC	&& ext_ccpart_mb_oib

************************************
* Funkcija: PRIK_PAR()
* Autor: Olivera Ilijev-∆ikoviÊ
* Opis : Prikaz podataka o ugovaratelju iz ponuda/polica na desni klik
***********************************
PROCEDURE prik_par
	PRIVATE ssel, sseldod, xvar, nulfunc, izjfunc, getfunc, xid_proj, xakcija, gpozovijmbg, avrs_part, ;
		avipstatus, xino, adrzave, apoziv_fix, apoziv_mob, xtpoziv_drz, xtpoziv_ptt, xtpoziv_str, xtbroj, ;
		xmpoziv_drz, xmpoziv_ptt, xmpoziv_str, xmbroj
	ssel = SELECT()
	*
	nulfunc = 'nulpartneri'
	izjfunc = 'izjpartneri'
	getfunc = 'getpartneri'
	*
	gpozovijmbg = ugo_id
	xino = 0
	STORE xdrz_poziv TO xtpoziv_drz, xmpoziv_drz
	STORE '' TO xtpoziv_ptt, xtpoziv_str, xtbroj, xmpoziv_ptt, xmpoziv_str, xmbroj
	*
	DIMENSION avrs_part[1], avipstatus[1], adrzave[1], apoziv_fix[1], apoziv_mob[1]

	fill_part_array()

	*!*	IF ml_file('vrs_part')
	*!*		ml_sql([SELECT '<standardni>' AS opis, ' ' AS sifra, 0 AS ino ]+;
	*!*			[ FROM sysset ]+;
	*!*			[ UNION ALL ]+;
	*!*			[ SELECT opis, sifra, ino FROM vrs_part ]+;
	*!*				[ INTO ARRAY avrs_part])
	*!*	ELSE
	*!*		DIME avrs_part(3,3)
	*!*		avrs_part(1,1)=['Standardni partner']
	*!*		avrs_part(1,2)=[' ']
	*!*		avrs_part(1,3)=[0]
	*!*		avrs_part(2,1)=['Strana osoba']
	*!*		avrs_part(2,2)=['S']
	*!*		avrs_part(2,3)=[1]
	*!*		avrs_part(3,1)=['Diplomatsko predstavniötvo']
	*!*		avrs_part(3,2)=['D']
	*!*		avrs_part(3,3)=[1]
	*!*	ENDIF

	*!*	IF ml_file('vip_stat')
	*!*		ml_sql([SELECT '< neodreeni >          ' AS opis, '  ' AS sifra ]+;
	*!*			[ FROM sysset ]+;
	*!*			[ UNION ALL ]+;
	*!*			[ SELECT opis, sifra FROM vip_stat ]+;
	*!*			IIF(xfqmat_kva, [ WHERE vip=?qgrupa_vip ],[])+;
	*!*			[ UNION ALL ]+;
	*!*			[ SELECT '\'+opis, sifra FROM vip_stat ]+;
	*!*			[ INTO ARRAY avipstatus])
	*!*	ELSE
	*!*		DIMENSION avipstatus(7,2)
	*!*		avipstatus(1,1)=[<neodreen>]
	*!*		avipstatus(1,2)=[  ]
	*!*		avipstatus(2,1)=[Standardni partner]
	*!*		avipstatus(2,2)=[S ]
	*!*		avipstatus(3,1)=[KljuËni partner (1)]
	*!*		avipstatus(3,2)=[V ]
	*!*		avipstatus(4,1)=[KljuËni partner (2)]
	*!*		avipstatus(4,2)=[V1]
	*!*		avipstatus(5,1)=[Zaposlenik]
	*!*		avipstatus(5,2)=[Z ]
	*!*		avipstatus(6,1)=[SteËaj]
	*!*		avipstatus(6,2)=[L ]
	*!*		avipstatus(7,1)=[PredsteËaj]
	*!*		avipstatus(7,2)=[P ]
	*!*	ENDIF
	*
	IF ml_sql([SELECT * FROM partneri WHERE JMBG=?GPOZOVIJMBG ]+;
			[INTO CURSOR ccdod_asp READWRITE])
		xakcija=IIF(_TALLY=0,'I','P')
		xakcija='P'
		sseldod=SELECT()
		xvar=pgc_var()
		PRIVATE &xvar
		STORE '' TO &xvar
		*
		IF xfqmat_cen
			nogrid((sseldod), xakcija, 'jmbg', , 'Podaci o partneru', 'partnerix')
		ELSE
			nogrid((sseldod), xakcija, 'jmbg', , 'Podaci o partneru', 'partnerix')
		ENDIF
		USE IN ccdod_asp
		*
	ENDIF
	*
	SELECT(ssel)
ENDPROC	&& prik_par

***************************************************************
PROCEDURE skadkonk
	skadkont('',.T.)
ENDFUNC && skadkonk

***************************************************************************
* Funkcija: fn_test1()
* Autor: Marko Bekafigo
* Opis : Test funkcija za seminar "Organizacija CUBIS sustava"
***************************************************************************
PROCEDURE fn_test1
abc_message('UPOZORENJE:'+CHR(13)+CHR(13)+'Poziv : FN_TEST1')
RETURN
ENDPROC && fn_test1

***************************************************************************
* Funkcija: fn_test2()
* Autor: Marko Bekafigo
* Opis : Test funkcija za seminar "Organizacija CUBIS sustava"
***************************************************************************
PROCEDURE fn_test2
abc_message('UPOZORENJE:'+CHR(13)+CHR(13)+'Poziv : FN_TEST2')
RETURN
ENDPROC && fn_test2

***************************************************************************
* Funkcija: fn_test3()
* Autor: Marko Bekafigo
* Opis : Test funkcija za seminar "Organizacija CUBIS sustava"
***************************************************************************
PROCEDURE fn_test3
abc_message('UPOZORENJE:'+CHR(13)+CHR(13)+'Poziv : FN_TEST3')
RETURN
ENDPROC && fn_test3

***************************************************************************
* Funkcija: fn_test4()
* Autor: Marko Bekafigo
* Opis : Test funkcija za seminar "Organizacija CUBIS sustava"
***************************************************************************
PROCEDURE fn_test4
abc_message('UPOZORENJE:'+CHR(13)+CHR(13)+'Poziv : FN_TEST4')
RETURN
ENDPROC && fn_test4

***************************************************************************
* Funkcija: fn_test5()
* Autor: Marko Bekafigo
* Opis : Test funkcija za seminar "Organizacija CUBIS sustava"
***************************************************************************
PROCEDURE fn_test5
abc_message('UPOZORENJE:'+CHR(13)+CHR(13)+'Poziv : FN_TEST5')
RETURN
ENDPROC && fn_test5

***************************************************************************
* Funkcija: fn_test6()
* Autor: Marko Bekafigo
* Opis : Test funkcija za seminar "Organizacija CUBIS sustava"
***************************************************************************
PROCEDURE fn_test6
abc_message('UPOZORENJE:'+CHR(13)+CHR(13)+'Poziv : FN_TEST6')
RETURN
ENDPROC && fn_test6

***************************************************************************
* Funkcija: skad_ob1
* Autor: Marko Bekafigo
* Opis : Skadencar - Izvjeötaj R1
***************************************************************************
PROCEDURE skad_ob1
abc_message('UPOZORENJE:'+CHR(13)+CHR(13)+'U izradi.')
RETURN
ENDPROC && skad_ob1

***************************************************************************
* Funkcija: skad_ob2
* Autor: Marko Bekafigo
* Opis : Skadencar - Izvjeötaj R2
***************************************************************************
PROCEDURE skad_ob2
abc_message('UPOZORENJE:'+CHR(13)+CHR(13)+'U izradi.')
RETURN
ENDPROC && skad_ob2

***************************************************************************
* Funkcija: skad_ob3
* Autor: Marko Bekafigo
* Opis : Skadencar - Izvjeötaj R3
***************************************************************************
PROCEDURE skad_ob3
abc_message('UPOZORENJE:'+CHR(13)+CHR(13)+'U izradi.')
RETURN
ENDPROC && skad_ob3

***************************************************************************
* Funkcija: skad_ob4
* Autor: Marko Bekafigo
* Opis : Skadencar - Izvjeötaj R4
***************************************************************************
PROCEDURE skad_ob4
abc_message('UPOZORENJE:'+CHR(13)+CHR(13)+'U izradi.')
RETURN
ENDPROC && skad_ob4


***************************************************************
* Naziv:	NEP_GOT
* Poziv:	DO NEP_GOT ili NEP_GOT()
* Namjena:	Izvjeötaj o evidentiranoj a nepoloûenoj gotovini
* Opis:		CRYSTAL REPORT
* Autor:	Zrinko
* Datum:	20.10.2012.
****************************************************************
PROCEDURE nep_got
PRIVATE ssel, xod, xdo, xid_sura, xnaziv_s, xid_admin, xnaziv_ad, xid_posl, xnaziv_f, ;
		xok, datafstring, xuvjet, xccparam, xnaslov_izv, xpodnaslov_izv
ssel=SELECT()
STORE '*' TO xid_sura, xid_admin, xid_posl
ml_sql([CREATE CURSOR ccparam(id_sura C(13), naziv_s C(50), id_admin C(8), naziv_ad C(50), od D, do D, ]+;
	[ id_posl C(10), naziv_f c(50), naslov_izv C(50), podnaslov_izv C(50))])
*
xok=.F.
xod=GOMONTH(DATE()-DAY(DATE())+1,-1)
xdo=GOMONTH(XOd,1)-1
xnaziv_s='svi suradnici'
xnaziv_ad='svi administratori'
xnaziv_f='sve filijale'
datafstring='nep_got'
DO FORM nep_got
xnaslov_izv='EVIDENTIRANA A NEPOLOéENA GOTOVINA'
IF xok=.T.
	xpodnaslov_izv='Razdoblje od '+DTOC(xod)+' do '+DTOC(xdo)
	ml_sql([INSERT INTO ccparam VALUES (?xid_sura, ?xnaziv_s, ?xid_admin, ?xnaziv_ad,  ]+;
		[  ?xod, ?xdo, ?xid_posl, ?xnaziv_f, ?xnaslov_izv, ?xpodnaslov_izv)])
		SELECT CCPARAM
	CursorToXML("ccparam","xccparam")
	USE IN ccparam
	xxml=qReportsObject.Gotovina_EvidentiranaNepolozena(xccparam)
	DO xml_out WITH xxml, 'NEP_GOT'
	RETURN
ENDIF
ENDPROC	&& nep_got

****************************************************************
FUNCTION  vratioznacene
PARAMETERS psto           && PSto='1' - vrati bez navodnika, PSto='2' - vrati s navodnicima
PRIVATE ssel, xret, xsvevrste, xoznacenevrste
ssel = SELECT()
*
SELECT t1
xsvevrste = RECCOUNT()
xret=[]
*
ml_sql([SELECT osnov FROM t1 WHERE oznaci INTO CURSOR ccoznacene])
xoznacenevrste = RECCOUNT()
SCAN
	xret = IIF(psto='1', xret+osnov+[,], xret+[']+osnov+[',])
ENDSCAN
USE IN ccoznacene
xret=IIF(xsvevrste = xoznacenevrste, '*', LEFT(xret,LEN(xret)-1))
*
SELECT (ssel)
RETURN xret

****************************************************************
* Lista opomena - za excel novi izgled 01.11.2012 - olivera-Poziv lst_opom sa parametrom
****************************************************************
PROCEDURE lstnopom
PRIVATE ssel
ssel=SELECT()
LST_OPOM(' ',.T.)
SELECT(ssel)
RETURN

***************************************************************
* olivera, 29.11.2012-izvjeötaj o obiljeûenim potraûivanjima
***************************************************************
PROCEDURE iz_otpis
PRIVATE ssel, xok, datafstring,Naslov_izv,Podnaslov_izv1,Podnaslov_izv2,;
	GDat_otpo,GDat_otpd,GDat_napo,GDat_napd,GDat_stoo,GDat_stod,seltmp, GTIP
ssel=SELECT()
Naslov_izv = 'Lista otpisanih / oznaËenih potraûivanja'
GTIP=''
STORE  DATE(YEAR(DATE()) - 1, 1, 1) TO GDat_otpo,GDat_napo,GDat_stoo
STORE DATE() TO GDat_otpd,GDat_napd,GDat_stod
Podnaslov_izv1 = 'Otpisano: -od ' +DTOC(Gdat_otpo) + ' do ' + DTOC(GDat_otpd)
Podnaslov_izv2 = 'NaplaÊeno  - od '+DTOC(Gdat_napo) + ' do ' + DTOC(GDat_napd)+', Stornirano: -od ' +DTOC(Gdat_stoo) + ' do ' + DTOC(GDat_stod)
*forma za unos parametara
xok=.F.
datafstring='otpis_uk'
*DO FORM iz_otpis NAME &datafstring
DO FORM otpis_uk NAME &datafstring
IF !xok
	SELECT (ssel)
	RETURN
ENDIF
xxml = qReportsObject.TisakPotrazivanja(DTOC(Gdat_otpo), DTOC(GDat_otpd),DTOC(Gdat_napo), DTOC(GDat_napd),DTOC(Gdat_stoo), DTOC(GDat_stod),GTip)
* tisak crystal
DO xml_out WITH xxml, 'OTPIS_UK'

SELECT (ssel)
RETURN


*******************************************************************************
FUNCTION getbrzah
PARAMETERS pgodina
PRIVATE ssel, retbr
ssel = SELECT()
*
ml_sql([SELECT MAX(broj) AS maxbrzah ]+;
	[FROM zah_odob WHERE broj LIKE ']+pGodina+[%' ]+;
	[INTO CURSOR tmpzah])
*
IF _TALLY>0 AND RECCOUNT()=1 AND !ISNULL(maxbrzah) AND !EMPTY(maxbrzah)
	retbr = PADL(ALLTRIM(STR(VAL(maxbrzah)+1)),8,'0')
ELSE
	retbr = pgodina+'000001'
ENDIF
USE IN tmpzah
*
SELECT (ssel)
RETURN retbr

***************************************************************************
* Funkcija: Svote_osig
* Autor: Goran (VFP) i Nadio (DLL)
* Datum: 15.01.2013.
* Opis: Izvjeötaj "Svote osiguranja po policama i prilozima"
***************************************************************************
PROCEDURE Svote_osig
PRIVATE SSel, XOd, XDo, XOnOff, XXML
SSel = SELECT()
XOd = DATE(YEAR(DATE()), 1, 1)
XDo = DATE(YEAR(DATE()), MONTH(DATE()), 1) - 1
XOnOff = SET('CURSOR')
SET CURSOR ON
PGC_RET2VAL('Svote osiguranja po policama i prilozima', 'Od datuma', 'Do datuma', 'XOd', 'XDo')
SET CURSOR &XOnOff
IF !EMPTY(XOd) AND !EMPTY(XDo)
	XXML = qReportsObject.Svote_osig(DTOC(XOd), DTOC(XDo))
	DO xml_out WITH XXML, 'SVOTE_OS'
ENDIF
SELECT (SSel)
RETURN
ENDPROC && Svote_osig


*******************************************************************************
* Incubis d.o.o.   	              unos_iban           (c) All rights reserved *
* Author......: Olivera					                     Date: 03.12.2009 *
* Description.: Funkcija za autom. unos IBAN-a u banke                        *
*******************************************************************************
FUNCTION unos_iban
PARAMETERS koja
PRIVATE ssel
ssel=SELECT()
IF koja='B'
	USE banka
	SCAN ALL
		REPLACE iban WITH qinsuranceobject.iban('HR', LEFT(br_racun,7), SUBSTR(br_racun,9,10))
	ENDSCAN
	USE IN banka
ENDIF
IF koja='O'
	USE opcine
	SCAN ALL
		REPLACE iban WITH qinsuranceobject.iban('HR', LEFT(ziroracun,7), SUBSTR(ziroracun,9,10))
		REPLACE iban1 WITH qinsuranceobject.iban('HR', LEFT(ziroprir,7), SUBSTR(ziroprir,9,10))
	ENDSCAN
	USE IN opcine
ENDIF
IF koja='P'
	USE porezi
	SCAN ALL
		REPLACE iban WITH qinsuranceobject.iban('HR', LEFT(ziroracun,7), SUBSTR(ziroracun,9,10))
	ENDSCAN
	USE IN porezi
ENDIF
SELECT (ssel)
RETURN


****************************************************************************
* Naziv:	rad_vr_valid
* Poziv:	rad_vr_valid()
* Namjena:	Provjera rada van radnog vremena ili neradnim danom
* Autor:	Marko
* Datum:	26.03.2013.
****************************************************************************
PROCEDURE rad_vr_valid
PARAMETERS pdate, ptime
PRIVATE xret
*
IF PCOUNT() = 0
    pdate = DATE()
    ptime = TIME()
ENDIF
RETURN DOW(pdate, 2) > 5 OR ptime > qrad_vrij_do
ENDPROC    && rad_vr_valid


***************************************************************
* Naziv:	Pripremi_nove_seriale_za_cursor
* Namjena:	Popunjavanje seriala u cursorima
* Autor:	Goran
* Datum:	25.07.2013.
***************************************************************
PROCEDURE Pripremi_nove_seriale_za_cursor
PARAMETERS pcursor, patribut
PRIVATE xsel
xsel = SELECT()
SELECT (pcursor)
DO Pripremi_ovdje_nove_seriale WITH patribut
SELECT (xsel)
RETURN
ENDPROC && Pripremi_nove_seriale_za_cursor
*
***************************************************************
* Namjena:	PomoÊna funkcija za popunjavanje seriala u cursorima
* Napomena:	Nije namjenjena za direktnu upotrebu (vidi funkciju iznad)
***************************************************************
PROCEDURE Pripremi_ovdje_nove_seriale
PARAMETERS patribut
PRIVATE xpreostalo, xserija
IF RECCOUNT() > 0
	GO TOP
	xpreostalo = RECCOUNT()
	DO WHILE !EOF() AND xpreostalo > 0
		xserija = MIN(xpreostalo, qser_maxb)
		xpreostalo = xpreostalo - xserija
		REPLACE &patribut WITH get_ser(xserija)
		SKIP
		IF xserija > 1
			REPLACE &patribut WITH get_ser() NEXT (xserija - 1)
			SKIP
		ENDIF
	ENDDO
ENDIF
RETURN
ENDPROC && Pripremi_ovdje_nove_seriale


*************************************************************************************************
* InCubis d.o.o.              	PROVJ_PART            				   (C) All rights reserved  *
* Author......: Zrinko PuljiÊ		                        				  Date: 26.08.2013  *
* Description.: Provjera podataka o poslovnom subjektu pomoÊu servisa						    *
*				- poziv kao samostalna funkcija (s menija) - bez parametara						*
*				- kao akcija nad partnerom - s parametrom 'PART'								*
*				- kao akcija nad policom/ponudom - s parametrom 'POL'							*
*************************************************************************************************
PROCEDURE provj_part
PARAMETERS pobj
PRIVATE ssel, xoib, xserv_url, xserv_user, xserv_pass, xresult
* ---
ssel=SELECT()
*
STORE '' TO xoib, xserv_url, xserv_user, xserv_pass, xresult
*
* postavke za spajanje na servis iz MEGALINE.INI
upd_env('', 'POSLOVNI.HR')
xserv_url = ALLTRIM(STRTRAN(PGCENV('SERV_URL'),'\'))
xserv_user = ALLTRIM(STRTRAN(PGCENV('SERV_USER'),'\'))
xserv_pass = ALLTRIM(STRTRAN(PGCENV('SERV_PASS'),'\'))
*
IF PCOUNT()=0 && poziv kao samostalna funkcija (OIB se ruËno unosi)
	DO WHILE !valid_oib(xoib) &&dok ne unese ispravan OIB
		xoib=PGC_RETVAL('Upiöite OIB poslovnog subjekta:',xoib)
		IF !valid_oib(xoib)
			abc_message('Upisali ste neispravan OIB!')
		ENDIF
	ENDDO
ELSE
	IF pobj=='PART' &&poziv s partnera na desni klik
		xoib=ALLTRIM(pdv_br)
	ENDIF
	IF  pobj=='POL' &&poziv s police/ponude na desni klik
		xoib=ALLTRIM(ret_ext(ugo_id,'PARTNERI',,'PDV_BR','JMBG'))
	ENDIF
ENDIF
*
* poziv servisa
qInsuranceObject.InitOIBService(xserv_url, xserv_user, xserv_pass)
xresult = qInsuranceObject.GetFormattedOIB(ALLTRIM(xoib))
xresult = STRTRAN(xresult, '</br>', CHR(13)) &&formatira se string s oznakom za novi red iz FOX-a
MESSAGEBOX(xresult, 64, 'Obavijest')
*
SELECT (ssel)
ENDPROC && provj_part


*************************************************************************************************
* InCubis d.o.o.              CUB_DMS                                  (C) All rights reserved  *
* Author......: Danijel »ubraniÊ                                              Date: 24.11.2017  *
* Description.: CUBIS - Document Management Service (WIENER)                                    *
*************************************************************************************************
PROCEDURE cub_dms
PRIVATE ssel, knjiga_str, claimnumber_str
ssel=SELECT()
knjiga_str=ALLTRIM(knjiga)
claimnumber_str=ALLTRIM(knjiga)+ALLTRIM(broj)
*
* poziv funkcije u dll-u
qInsuranceObject.CubisDMS_ClaimDataSend(knjiga_str, claimnumber_str)
abc_message(qInsuranceObject.Error)
*
SELECT(Ssel)
ENDPROC &&cub_audatex

*************************************************************************************************
* InCubis d.o.o.              CUB_DMS2                                 (C) All rights reserved  *
* Author......: Danijel »ubraniÊ                                              Date: 22.10.2019  *
* Description.: CUBIS - Document Management Service ver2 sa proöirenim slogom i veÊim           *
* brojem knjiga öteta ( ötete iz grupe AO, AK, IM, OO, TK) (WIENER)                             *
*************************************************************************************************
PROCEDURE cub_dms2
PRIVATE ssel, knjiga_str, claimnumber_str
ssel=SELECT()
knjiga_str=ALLTRIM(knjiga)
claimnumber_str=ALLTRIM(knjiga)+ALLTRIM(broj)
*
* poziv funkcije u dll-u
qInsuranceObject.CubisDMS_ClaimDataSend2(knjiga_str, claimnumber_str)
abc_message(qInsuranceObject.Error)
*
SELECT(Ssel)
ENDPROC &&cub_audatex

*************************************************************************************************
* InCubis d.o.o.              CUB_AUDATEX                              (C) All rights reserved  *
* Author......: Maja JahiÊ                                                    Date: 20.01.2014  *
* Description.: CUBIS - Interface Audatex (UNIQA)                                               *
*************************************************************************************************
PROCEDURE cub_audatex
PRIVATE ssel, knjiga_str, claimnumber_str
ssel=SELECT()
knjiga_str=ALLTRIM(knjiga)
claimnumber_str=ALLTRIM(knjiga)+ALLTRIM(broj)
*
* poziv funkcije u dll-u
qInsuranceObject.CubisAudatex_ClaimDataSend(knjiga_str, claimnumber_str)
abc_message(qInsuranceObject.Error)
*
SELECT(Ssel)
ENDPROC &&cub_audatex

*************************************************************************************************
* InCubis d.o.o.              CUB_AUDATIM                              (C) All rights reserved  *
* Author......: Danijel »ubraniÊ                                              Date: 20.12.2016  *
* Description.: CUBIS - Interface Audatex (WIENER)                                              *
*************************************************************************************************
PROCEDURE cub_audatim
PRIVATE ssel, knjiga_str, claimnumber_str,xbroj,xresult
ssel=SELECT()
knjiga_str=ALLTRIM(knjiga)
claimnumber_str=ALLTRIM(knjiga)+ALLTRIM(broj)
xbroj=claimnumber_str

*
* poziv funkcije u dll-u
*

xresult=qInsuranceObject.CubisAudatex_ClaimDataReceive(knjiga_str, claimnumber_str,xbroj)
IF !EMPTY(qInsuranceObject.Error)
	abc_message(qInsuranceObject.Error)
ELSE
	abc_message(xresult)
ENDIF

*
SELECT(Ssel)
ENDPROC &&cub_audatex

***************************************************************
* Naziv:	Promjena VIP statusa
* Poziv:	PROMJ_VIP(1) ili PROMJ_VIP(2)
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	03.03.2014
* Promjene: 18.05.2018. onemoguÊena promjena VIP statusa ako je prazan OIB ili partner sorniran
***************************************************************
FUNCTION promj_vip
	PARAMETERS grupa
	PRIVATE ssel, xy, xvip_status, xret, xser, gok, xnaziv_status, xgrupa
	xret=.F.
	gok=.F.
	xy=RECNO()
	xgrupa=IIF(PCOUNT()=0, 1, grupa)
	ssel=SELECT()
	IF qoib_vip
		xstatusold=vip_status
		xpdv_br=pdv_br
		IF EMPTY(xpdv_br)
			abc_message('UPOZORENJE:'+CHR(13)+CHR(13)+'Partner nema upisan OIB, nije moguÊa promjena VIP statusa!')
			SELECT (ssel)
			RETURN xy
		ENDIF
		xvip_status=vip_status
		xnaziv_status=ret_xt1('opis','vip_stat', 'sifra=?xvip_status', 0)
		ml_sql([CREATE CURSOR tmpskup (izabran L(1), jmbg C(13), pdv_br C(11), mat_broj C(13), ]+ ;
			[naziv C(60), ino_por_br C(20), status C(1), vip_status C(2))])
		seltmp=SELECT()
		ml_sql([SELECT jmbg, pdv_br, mat_broj, naziv, ino_por_br, status, vip_status ]+;
			[ FROM partneri ]+;
			[ WHERE pdv_br=?xpdv_br AND status<>'9' ]+;
			[ ORDER BY naziv ]+;
			[ INTO CURSOR t100])
		IF _TALLY>0
			SELECT tmpskup
			APPEND FROM DBF('t100')
		ELSE
			abc_message('UPOZORENJE:'+CHR(13)+CHR(13)+'Partner je storniran, nije moguÊa promjena VIP statusa!')
			SELECT (ssel)
			RETURN xy
		ENDIF
		USE IN t100
		SELECT tmpskup
		SCAN ALL
			REPLACE izabran WITH .T.
		ENDSCAN
		GO TOP
		*
		datafstring='promj_vip'
		DO FORM promj_vip  NAME &datafstring
		*
		IF gok AND !EMPTY(xpdv_br) AND (RET_XT1('COUNT(*)', 'vip_stat', 'sifra=?xvip_status', 0) > 0)
			IF xstatusold=xvip_status
				RETURN
			ELSE
			SELECT (seltmp)
			* inicijalno su svi partneri izabrani
			SCAN ALL FOR izabran=.T.
				xjmbg=jmbg
				ml_sql([UPDATE partneri SET vip_status=?xvip_status WHERE pdv_br=?xpdv_br AND jmbg=?xjmbg])
			ENDSCAN
				xser=GET_SER()
				ml_sql([INSERT INTO stat_oib (serial, oib, status, datum, korisnik) ] +;
					[ VALUES (?xser, ?xpdv_br, ?xvip_status, ?DATE(), ?muser)])
				IF RET_XT1('COUNT(*)', 'oib_vip', 'oib=?xpdv_br', 0) > 0
					ml_sql([UPDATE oib_vip SET status=?xvip_status WHERE oib=?xpdv_br])
				ELSE
					ml_sql([INSERT INTO oib_vip (oib, status) VALUES (?xpdv_br, ?xvip_status)])
				ENDIF
			ENDIF
			xret=.T.
		ENDIF
	ELSE
		abc_message('Nedostaje parametar u megaline.ini za novi naËin odreivanja vip statusa partnera.')
	ENDIF
	SELECT (ssel)
	GOTO xy
	RETURN xret
ENDFUNC && promj_vip

****************************************************************************
* Naziv:	promjena_ps
* Poziv:	promjena_ps()
* Namjena:	Privremena prodajna struktura
*			Dozvola promjene PS-e korisniku koji ima ukljuËenu ovlast PROMPS
* Autor:	Marko Bekafigo
* Datum:	04.09.2013.
****************************************************************************
PROCEDURE promjena_ps
promjena_ps1(qsuradnik, qprodstru)
RETURN

****************************************************************************
PROCEDURE promjena_ps1
PARAMETERS psuradnik, pprodstru
PRIVATE ssel, aprodstru, xok, xps, xnaz_sura
* ---
ssel = SELECT()
*
xok = .F.
IF ('+PROMPS+'$qovlasti) AND !EMPTY(pprodstru)
	xps = pprodstru
	xnaz_sura = ret_ext(psuradnik, 'suradnci', , 'imeipre', 'id_sura')
	*
	DIMENSION aprodstru(1)
	ml_sql([SELECT id_ps, opis ]+;
		[ FROM prodstru ]+;
		[ WHERE id_nad_ps=?pprodstru OR id_nad_ps IN (SELECT id_nad_ps FROM prodstru WHERE id_ps=?pprodstru) ]+;
		[ INTO CURSOR tmpprodstru])
	ml_sql([SELECT '('+ALLTRIM(id_ps)+')'+opis AS ps, id_ps ]+;
		[ FROM tmpprodstru ]+;
		[ ORDER BY id_ps INTO ARRAY aprodstru])
	IF _TALLY>0
		SHW_FORM(xok, 'PROMPS', 'Privremena prodajna struktura', 'Suradnik: '+ALLTRIM(psuradnik)+' - '+ALLTRIM(xnaz_sura), ;
			'MatiËna prodajna struktura:','TT_C', 'pprodstru', 'xps', 'Privremena prodajna struktura:')
		IF xok AND xps<>pprodstru
			pprodstru = xps
		ENDIF
	ENDIF
	USE IN tmpprodstru
ENDIF
*
SELECT (ssel)
RETURN xok
ENDFUNC	&& promjena_ps1


****************************************************************************
* Naziv:	fnv
* Poziv:	fnv(), primjer fnv('kartpol')
* Namjena:	Poziv akcije nad view-om
* Autor:	Marko Bekafigo
* Datum:	22.11.2013.
* Izmjena:	28.02.2019 (Marko) - omoguÊen poziv bilo koje akcije nad view-om ponuda ili polica
****************************************************************************
PROCEDURE fnv
PARAMETERS pfunc
PRIVATE ssel, xproizvod, xbroj, xvar, ptip
ssel = SELECT()
*
IF VARTYPE(pview)='C' AND ('POLICE'$pview OR 'PONUDE'$pview)	&& INLIST(pview, 'POLICE', 'PONUDE')
	ptip = pview
	xproizvod = proizvod
	xbroj = broj

	ml_sql([SELECT * FROM skup_os WHERE sifra=?xproizvod INTO CURSOR tmpskup_os])
	xvar = pgc_var('S','*')+', sforma'
	PRIVATE &xvar
	STORE '' TO &xvar
	izj_nul('IZJ', 'S', '*')
	USE IN tmpskup_os

	ml_sql([SELECT * FROM ]+IIF(ptip='POLICE', stab_pol, stab_pon)+[ WHERE broj=?xbroj INTO CURSOR ccpolpon])
	xvar = pgc_var('G','*')
	PRIVATE &xvar
	STORE '' TO &xvar
	izj_nul('IZJ', 'G', '*')

	* dio za odvajanje tablica ponuda i polica
	PRIVATE xtip_pp, xnaslov, stablica, sforma, xtab_memo, xtab_visegod, xtab_stavke, xtab_priz, xtab_prem, xtab_kred, ;
		xtab_pril, xtab_dosi, xtab_pred, xtmp_pred, xtab_status, xgugo_id, xvrsta_old, xgreg_old, xgugo_old, xgosi_old, ;
		xtab_dost, xtab_korvoz, olddt, xgod_pro_old, xbonus_old, xakcnez, xtab_polv, xtab_stpolv, xtab_polr, xtab_idreos, ;
		xtab_plat, xtab_pml, xtab_sa, xtab_uvjp

	xtip_pp = ptip
	IF ptip='POLICE'
		xnaslov='Police - osiguranja '+snaslov
		stablica=stab_pol
	ELSE
		xnaslov='Ponude - osiguranja '+snaslov
		stablica=stab_pon
	ENDIF

	STORE '' TO xgugo_id, xvrsta_old, xgreg_old, xgugo_old, xgosi_old,xakcnez
	STORE {} TO olddt
	STORE 0 TO xgod_pro_old, xbonus_old
	IF ptip='POLICE'
		xtab_memo = 'POLI_TXT'
		xtab_visegod = 'VISEGOD'
		xtab_stavke = 'ST_POL'
		xtab_priz = 'POL_RIZ'
		xtab_prem = 'ST_PREM'
		xtab_kred = 'ST_KRED'
		xtab_pril = 'ST_PRIL'
		xtab_dosi = 'POLOSIG'
		xtab_pred = IIF(!EMPTY(SPred_pol),SPred_pol,'IMOPRPOL')
		xtmp_pred = 'TMPPREDPOL'
		xtab_status = 'STAT_POL'
		xtab_dost = 'POL_DOST'
		xtab_korvoz = 'POL_KORV'
		xtab_polv = 'POL_VAL'
		xtab_stpolv = 'ST_POLVAL'
		xtab_polr = 'POL_REOS'
		xtab_idreos = 'POL_IDREOS'
		xtab_plat = 'POL_PLAT'
		xtab_pml = 'ST_PMLPOL'
		xtab_sa = 'ST_SA_POL'
		xtab_uvjp = 'UVJ_POL'
	ELSE
		xtab_memo='PONU_TXT'
		xtab_visegod='VISEGOP'
		xtab_stavke='ST_PON'
		xtab_priz='PON_RIZ'
		xtab_prem='ST_PPON'
		xtab_kred='ST_KRED'
		xtab_pril='ST_PRIN'
		xtab_dosi='PONOSIG'
		xtab_pred=IIF(!EMPTY(SPred_pon),SPred_pon,'IMOPRPON')
		xtmp_pred='TMPPREDPON'
		xtab_status='STAT_PON'
		xtab_dost='PON_DOST'
		xtab_korvoz = 'PON_KORV'
		xtab_polv = 'PON_VAL'
		xtab_stpolv = 'ST_PONVAL'
		xtab_polr = 'PON_REOS'
		xtab_idreos = 'PON_IDREOS'
		xtab_plat = 'PON_PLAT'
		xtab_pml = 'ST_PMLPON'
		xtab_sa = 'ST_SA_PON'
		xtab_uvjp = 'UVJ_PON'
	ENDIF

	* sfrm_pon samo sa prva 3 CASE-a + 03,10,A1,19,20,23,'IL',RP a za ostale sfrm_pol
	DO CASE
		CASE ssifra='01' AND ((ptip='POLICE' AND INLIST(sfrm_pol,'POLI01U','POLI01CA')) OR ;
			(ptip='PONUDE' AND INLIST(sfrm_pon,'PONU01U','PONU01CA')))
			sforma = IIF(ptip='PONUDE',sfrm_pon,sfrm_pol)
		CASE ssifra='N1' AND ((ptip='POLICE' AND sfrm_pol='POLIN1U') OR (ptip='PONUDE' AND sfrm_pon='PONUN1U'))
			sforma = IIF(ptip='PONUDE',sfrm_pon,sfrm_pol)
		CASE ssifra='2B' AND ((ptip='POLICE' AND sfrm_pol='POLI2BU') OR (ptip='PONUDE' AND sfrm_pon='PONU2BU'))
			sforma = IIF(ptip='PONUDE',sfrm_pon,sfrm_pol)
		CASE ssifra='2V' AND ((ptip='POLICE' AND sfrm_pol='POLI2VU') OR (ptip='PONUDE' AND sfrm_pon='PONU2VU'))
			sforma = IIF(ptip='PONUDE',sfrm_pon,sfrm_pol)
		OTHERWISE
			sforma = IIF(ptip='PONUDE' AND (sosn_vrst='03' OR (sosn_vrst='10' AND !INLIST(ssifra, 'A2', 'PO', '30')) OR INLIST(ssifra,'19','20','23','IL','RP')), sfrm_pon, sfrm_pol)
	ENDCASE

	* kretanje po modelu
	IF !EMPTY(povname) AND povname=CHR(1)
		PRIVATE sosn_vrst,ssifra,pbroj
		stablica = ALLTRIM(rel[nIndex,3])
		IF ptip='POLICE'
			sforma='POLICE'
		ELSE
			sforma='PONUDE'
		ENDIF
		STORE ' ' TO sosn_vrst, ssifra, pbroj
	ENDIF

	PRIVATE selpol, selpol1, seltmp, seltmpr, seltmpk, seltmpp, seltmpv, seltmpvk, seltmpreos, seltmppml, seltmpsa, seltmpuvjp, ;
		seltmpos, seltmpna, seltmpsp, tmp_file, tmp_alias, tmp_rate, tmp_pril, tmpbroj, oldranija, gnaziv, gnac, gprov, grd, ;
		gprev, gruvrstmp, gidgru, gid_prem, grnapl, gzbroj, gumnozak, gkomerc, gpopdop, gpredmet, gprilog_txt, gpoc_osig, ;
		gist_osig, giznos_c, gnapomena, galt_vrsta, galt_iznos, guvjeti_poz, gkoefkr, gozn_tab_kr, gozn_pop, gnaz_pop, gizn_pop, ;
		gval, gkor_fakt, gdokum2, gstat_odob, gdat_rod, gdat_rod2, gextra_an, xpgcvars, xmin_prem, xponudalp, ximnap, xvinkulac, ;
		xpomoc_v, xrezul_v, xref_val, xstari_id_agen, xstari_id_sura, xstari_ugo_id, xzpr, xserizv, xizvod, xdat_pla, xnerasp, ;
		xpartner, xranija0, xpolpon, xid_vrst_pz, ximapred, xosnovica, xcj_kat, xcj_opr, xcj_ppmv, xranoli, xsif_akc, xdok_akc1, ;
		xdok_akc2, xdok_akc3, xdok_akc4, xdok_akc5, xnap_akc, xbr_dok, xkey_akc1, xkey_akc2, xkey_akc3, xkey_akc4, xkey_akc5, ;
		xbr_god, xpoc_dat_old, xist_dat_old, xistidatum, gpop3_o, gpop4_o, gpop5_o, gpop6_o, gpop1_i, gpop2_i, gpop3_i, gpop4_i, ;
		gpop5_i, gpop6_i, gpop1_izn, gpop2_izn, gpop3_izn, gpop4_izn, gpop5_izn, gpop6_izn, gdop1_izn, gdop2_izn, g1, g2, ;
		g3, g4, g5, g6, g7, g8, xrata_old, xpla_id_old, gnasao_policu, xdosync, xbonushuo, gdor, gid_rpp, arpp_zoro, abojars, ;
		xvrsta_old, gsrc, xsura_old, xuvj_poz_old, gdost_naziv, gdost_adr, gdost_ptt, gdost_mje, xrata_anex, xpla_id_anex, ;
		astup_bon, gao_paket, gao_paket_old, xid_agen_old, xsektor_old, seltmpdop, seltmppop, xgod_pro_old, gasist_paket, ;
		gasist_paket_old, xextra_an, xan_komb, xbonus_old, xctrl_se, gtip_gru, xunipdvnew, gser_polval, gdrzava, gvaluta, ;
		gtecaj_pol, xid_vrspolre, gizn_prem_uk, gizn_kom_uk, gino_por_uk, xval_pol, xval_pol_old, xid_reos, xid_vrsreug, ;
		xid_vrsreobl, xid_fakul, xid_oblig, xudio_re, xbr_kart, xistek, xunipdunos, xranija_old, xid_cjen_old, xuk_prem_val, ;
		xstat, xtip_reos, xprem_fp, xprem_ok, xprem_sp1, xprem_sp2, xprov_fp, xprov_ok, xprov_sp1, xprov_sp2, xkap_fak, ;
		xkap_prop, xlok_brok, xosn_prem_o, xosn_prem_f, xprem_fxl, xre_prov_fak, xfran_fxl, xfran_hrk, xudio_fak, xudio_kv, ;
		xudio_sp1, xudio_sp2, xbr_fakpost, xizn_br_fp, xid_reoblgod, xid_reoblgod2, xindakckoj, xprom_lom, xiban, xproc_old, ;
		xsds, xadm_part, gapass_paket, gapass_paket_old, xdi_obnpol, xdz_obnpol, xobn_di_pop, xobn_dz_pop, xbr_god_old, ;
		xpoctra_old, xindpoctra, xprop_udio, xprop_prov, xsurp1_prov, xsurp2_prov, xid_vrsreobl_old, xid_vrsreug_old, ;
		xvjer05, xvjer10, xvjer15, xvjer20, xobn05, gspol2, tunikom, xbroj_pol_re, xbrok_ino, xbrok_ino_p, xbrok_hr, ;
		xbrok_hr_p, xre_prov_ff, xviu_prov, xizn_ispl_re, xizn_vd, xprem_bvd, xizn_udio_re, xbrok_ino_izn, xbrok_hr_izn, ;
		xre_izn_ff, xviu_izn, xre_izn_ispl, xobn_cjen_pop, xid_plat, xposto_reos1, xposto_reos2, xposto_reos3, btn_izrac, ;
		xtrig_dzo, aosigur, apremst, xizb_cjen, gizb_cjen, xpozovi_cjenu, xan_komb_nove_svote, gjom, gvolumen, gsnaga, gndm, ;
		gbr_sjeda, gbr_vrata, gbr_motor, gpr_reg, gkm, goblik_kar, gvrs_motor, gvrs_mjenjac, gvrs_dod_mot, gvrs_pogon, ;
		gvrs_ovjes, gpaket_opr, geko_program, gco2, gelek_doseg, gduljina, gmasa, gmaterijal, gboja, xpremija_god, xpremija_pr, ;
		xporez_god, xizn_prem_god, xizn_prem_pr, xizn_por_god

	STORE ' ' TO selpol, selpol1, seltmp, seltmpr, seltmpk, seltmpp, seltmpv, seltmpvk, seltmpreos, seltmppml, seltmpsa, ;
		seltmpuvjp, seltmpos, seltmpna, seltmpsp, tmp_file, tmp_alias, tmp_rate, tmp_pril, gnaziv, gnac, gprov, gruvrstmp, ;
		gidgru, gid_prem, grnapl, gzbroj, gumnozak, gkomerc, gpredmet, gprilog_txt, gpoc_osig, gist_osig, giznos_c, gnapomena, ;
		galt_vrsta, galt_iznos, guvjeti_poz, gkoefkr, gozn_tab_kr, gozn_pop, gnaz_pop, gizn_pop, gval, gdokum2, gstat_odob, ;
		xpgcvars, xmin_prem, xponudalp, xvinkulac, xpomoc_v, xrezul_v, xref_val, xstari_id_agen, xstari_id_sura, xstari_ugo_id, ;
		xizvod, xpartner, tmpbroj, xranija0, xid_vrst_pz, oldranija, xranoli, xsif_akc, xdok_akc1, xdok_akc2, xdok_akc3, ;
		xdok_akc4, xdok_akc5, xnap_akc, xbr_dok, xkey_akc1, xkey_akc2, xkey_akc3, xkey_akc4, xkey_akc5, gpop3_o, gpop4_o, ;
		gpop5_o, gpop6_o, xpla_id_old, gid_rpp, arpp_zoro, abojars, xvrsta_old, xsura_old, xuvj_poz_old, gdost_naziv, ;
		gdost_adr, gdost_ptt, gdost_mje, xrata_anex, xpla_id_anex, gao_paket, gao_paket_old, xid_agen_old, xsektor_old, ;
		seltmpdop, seltmppop, gasist_paket, xan_komb, gasist_paket_old, gtip_gru, gdrzava, gvaluta, xid_vrspolre, ;
		xbroj_pol_re, xbrok_ino, xbrok_hr, xid_reos, xid_vrsreug, xid_vrsreobl, xid_fakul, xid_oblig, xbr_kart, xistek, ;
		xranija_old, xid_cjen_old, xstat, xtip_reos, xindakckoj, xiban, xsds, xadm_part, gapass_paket, gapass_paket_old, ;
		xobn_di_pop, xobn_dz_pop, xid_vrsreobl_old, xid_vrsreug_old, xvjer05, xvjer10, xvjer15, xvjer20, xobn05, gspol2, ;
		xobn_cjen_pop, xid_plat, xan_komb_nove_svote, gjom, gbr_motor, goblik_kar, gvrs_motor, gvrs_mjenjac, gvrs_dod_mot, ;
		gvrs_pogon, gvrs_ovjes, gpaket_opr, geko_program, gmaterijal, gboja, gpodvrsta, goblikkar

	STORE 0 TO grd, gprev, xzpr, xserizv, xnerasp, gpopdop, xcj_kat, xosnovica, xcj_opr, xcj_ppmv, gextra_an, gpop1_i, ;
		gpop2_i, gpop3_i, gpop4_i, gpop5_i, gpop6_i, gpop1_izn, gpop2_izn, gpop3_izn, gpop4_izn, gpop5_izn, gpop6_izn, ;
		gdop1_izn, gdop2_izn, g1, g2, g3, g4, g5, g6, g7, g8, xrata_old, xbonushuo, gdor, xgod_pro_old, xbonus_old, ;
		xextra_an, xctrl_se, gtecaj_pol, gizn_prem_uk, gizn_kom_uk, gino_por_uk, xval_pol, xval_pol_old, gser_polval, ;
		xudio_re, xuk_prem_val, xprem_fp, xprem_ok, xprem_sp1, xprem_sp2, xprov_fp, xprov_ok, xprov_sp1, xprov_sp2, ;
		xkap_fak, xkap_prop, xlok_brok, xosn_prem_o, xosn_prem_f, xprem_fxl, xre_prov_fak, xfran_fxl, xfran_hrk, ;
		xudio_fak, xudio_kv, xudio_sp1, xudio_sp2, xbr_fakpost, xizn_br_fp, xid_reoblgod, xid_reoblgod2, xproc_old, ;
		xbr_god_old, xprop_udio, xprop_prov, xsurp1_prov, xsurp2_prov, xbrok_ino_p, xbrok_hr_p, xre_prov_ff, xviu_prov, ;
		xizn_ispl_re, xizn_vd, xprem_bvd, xizn_udio_re, xbrok_ino_izn, xbrok_hr_izn, xre_izn_ff, xviu_izn, xre_izn_ispl, ;
		xposto_reos1, xposto_reos2, xposto_reos3, gnosivost, gvolumen, gsnaga, gndm, gbr_sjeda, gbr_vrata, gkm, gco2, ;
		gelek_doseg, gduljina, gmasa, xpremija_god, xpremija_pr, xporez_god, xizn_prem_god, xizn_prem_pr, xizn_por_god

	STORE .F. TO xvinkulac, ximnap, ximapred, xistidatum, gnasao_policu, xdosync, xunipdunos, xprom_lom, xdi_obnpol, ;
		xdz_obnpol, xtrig_dzo, xpozovi_cjenu

	STORE qempty_date TO gpr_regxdat_pla, xpoc_dat_old, xist_dat_old, xpoctra_old
	STORE {} TO gdat_rod, gdat_rod2, xunipdvnew

	xpolpon = .T.
	xindpoctra = .F.
	btn_izrac=.F.
	gkor_fakt = 1
	xbr_god = 1
	tunikom = 1
	gsrc = 'W'
	gpop5_o = xposebni_sif
	gpop6_o = xbudget_sif
	xtrig_dzo = (xfqmat_tri AND ssifra$xproizvod_dzo)
	IF ptip='POLICE'
		xctrl_se = IIF(EMPTY(spol_se), 0, ret_ext(spol_se, 'dokum_se', ,'ctrl_se', 'oznaka'))
	ELSE
		xctrl_se = IIF(EMPTY(spon_se), 0, ret_ext(spon_se, 'dokum_se', ,'ctrl_se', 'oznaka'))
	ENDIF

	PRIVATE niz_pla, niz_din, niz_din2
	DIMENSION niz_pla(1), niz_din(1), niz_din2(1), astup_bon(1,2), aosigur(1,2), apremst(1,2)
	STORE '' TO niz_pla(1), niz_din(1), niz_din2(1)
	astup_bon(1,1) = ' '
	astup_bon(1,2) = 0
	aosigur(1,1) = ' '
	apremst(1,2) = ' '

	ml_sql([SELECT sifra, opis FROM status_p INTO CURSOR tmpbrowstat_p])

	ml_sql([SELECT pla_id, naziv ]+ ;
		[ FROM placanja ]+ ;
		[ WHERE aktivno=1 ]+IIF(qnac_plac='ON', [AND nakt=1], [])+ ;
		[ ORDER BY pla_id INTO ARRAY niz_pla])

	IF (ssifra='01' AND ((ptip='POLICE' AND sfrm_pol='POLI01CA') OR (ptip='PONUDE' AND sfrm_pon='PONU01CA'))) ;
		OR (INLIST(ssifra, '2B', '2V', '20') AND xfqmat_uni) ;
		OR  INLIST(ssifra, '19', '23', 'IL', 'RP') OR (ssifra='20' AND xfqmat_jah)
		ml_sql([SELECT sifra, naziv, mjeseci, kolxgod FROM din_pla WHERE sifra<>'00' ORDER BY sifra INTO ARRAY niz_din])
		ml_sql([SELECT naziv, sifra, mjeseci, kolxgod FROM din_pla WHERE sifra<>'00' ORDER BY naziv INTO ARRAY niz_din2])
	ELSE
		ml_sql([SELECT sifra, naziv, mjeseci, kolxgod FROM din_pla ORDER BY sifra INTO ARRAY niz_din])
		IF (ssifra='01' AND ((ptip='POLICE' AND sfrm_pol='POLI01U') OR (ptip='PONUDE' AND sfrm_pon='PONU01U'))) ;
			OR INLIST(ssifra,'1P','1C','1T','N1')
			ml_sql([SELECT naziv, sifra, mjeseci, kolxgod FROM din_pla ORDER BY naziv INTO ARRAY niz_din2])
		ENDIF
	ENDIF
	gkolxgod = niz_din[1,4]
	galt_vrsta = SPACE(8)
	galt_iznos = 0
	IF sporez<>0 OR salt_porez<>0
		qporez = sporez
		qalt_porez = salt_porez
	ENDIF

	* pgc_form
	PRIVATE atrfunc, nulfunc, izjfunc, getfunc, izjfunc1, setfunc
	setfunc = "SETPOLPON('"+ptip+"')"
	atrfunc = "ATRPOLICE('"+ptip+"')"
	nulfunc = "NULPOLICE('"+ptip+"')"
	izjfunc = "IZJPOLICE('"+ptip+"')"
	getfunc = "GETPOLICE('"+ptip+"')"
	izjfunc1 = izjfunc	&&zbog PGC_SMART-a	 u njemu se Deklarira, Briöe IzjFunc

	* pgc_stavke
	PRIVATE f_list, f_pict, f_desc, f_pred, f_vali, f_font, f_width, dup_izj, dup_rep, new_row, pre_upd, ;
		pos_upd, pos_del, pre_del, defwhen, deffont, defvali, can_app, brpo, brmax
	STORE 'PNE()' TO dup_izj, dup_rep, new_row, pre_upd, pos_upd, pre_del, pos_del, defwhen, defvali
	can_app = .F.
	deffont = 8
	DO CASE
		CASE ssifra=='01' AND ((ptip='POLICE' AND INLIST(sfrm_pol,'POLI01U','POLI01CA')) OR ;
			(ptip='PONUDE' AND INLIST(sfrm_pon,'PONU01U','PONU01CA')))
			brpo=9
		CASE (xfqmat_uni AND ssifra='SP') OR ssifra$xproizvod_dzo OR (xfqmat_kva AND ssifra='SM')		&& (xfqmat_uni AND ssifra='SP') OR ssifra$xproizvod_dzo		&& (xfqmat_uni AND INLIST(ssifra,'SP','DI')) OR (xfqmat_tri AND ssifra='DT')
			brpo=11
		OTHERWISE
			brpo=10
	ENDCASE
	brmax = 07
	DIMENSION f_list[brpo], f_pict[brpo], f_desc[brpo], f_pred[brpo], f_vali[brpo], f_font[brpo], f_width[brpo]

	SELECT ccpolpon
	DO Specificnosti_proizvoda IN police WITH 'fnv_nastavak'
	USE IN ccpolpon
ELSE
	abc_message(pgc_dlang('UPOZORENJE')+CHR(13)+CHR(13)+;
		pgc_dlang('Dozvola poziva akcija samo nad view-om ponuda ili polica'))
	RETURN .F.
ENDIF
*
SELECT (ssel)
RETURN .T.
ENDFUNC	&& fnv


****************************************************************************
* Naziv:	fnv_nastavak
* Autor:	Marko Bekafigo
* Datum:	04.03.2019
****************************************************************************
PROCEDURE fnv_nastavak
PRIVATE ssel
ssel = SELECT()
*
create_stavke_pp()
*
SELECT (ssel)
IF '('$pfunc
	&pfunc
ELSE
	DO &pfunc
ENDIF
*
destroy_stavke_pp()
*
SELECT (ssel)
RETURN .T.
ENDFUNC	&& fnv_nastavak


***************************************************************
* Naziv:	Glavni administrator SE
* Namjena:	VraÊa informaciju oznaËava li parametar glavnog administratora SE
* Autor:	Goran
* Datum:	26.11.2013.
***************************************************************
PROCEDURE FGl_admin_SE
PARAMETERS ps_admin_se
RETURN (LEFT(ps_admin_se, 3) == '00 ' AND (xfqmat_bih OR xfqmat_izv)) ;
	OR (LEFT(ps_admin_se, 3) == '99 ' AND xfqmat_kva)
ENDPROC && FGl_admin_SE


***************************************************************
* Naziv:	Varijable za tisak
* Namjena:	Popunjava array naizmjence imenima i vrijednostima
* 			varijabli Ëija su imena poslana u prvom parametru
* Autor:	Goran
* Datum:	03.12.2013.
***************************************************************
PROCEDURE FVarTisak
PARAMETERS p_atributi, p_ar
PRIVATE i, xlen
xlen = GETWORDCOUNT(p_atributi)
DIMENSION p_ar[xlen * 2]
FOR i = 1 TO xlen
	p_ar[i + i - 1] = GETWORDNUM(p_atributi, i)
	p_ar[i + i] = EVALUATE(p_ar[i + i - 1])
ENDFOR
RETURN
ENDPROC && FVarTisak


***************************************************************
* Naziv:	VratiTocneGodine
* Opis: 	VraÊa starost po datumu roenja
* Autor:	Goran i Olivera
* Datum:	07.02.2014.
***************************************************************
FUNCTION VratiTocneGodine
PARAMETERS pdatrod, pdatum
PRIVATE xgodina
xgodina = YEAR(pdatum) - YEAR(pdatrod)
IF GOMONTH(pdatrod, xgodina * 12) > pdatum
	xgodina = xgodina - 1
ENDIF
RETURN xgodina
ENDFUNC && VratiTocneGodine

***************************************************************
* Naziv:	UKojojJeGodiniZivota
* Opis: 	VraÊa u kojoj je godini ûivota po datumu roenja
* Autor:	Goran i Marko
* Datum:	12.03.2014.
***************************************************************
FUNCTION UKojojJeGodiniZivota
PARAMETERS pdatrod, pdatum
RETURN VratiTocneGodine(pdatrod, pdatum - 1) + 1
ENDFUNC && UKojojJeGodiniZivota

***************************************************************
* Naziv:	Monitor dnevne produkcije (automatska varijanta)
* Poziv:	MON_DPR()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	22.04.2014
***************************************************************
PROCEDURE mon_dpr
DO mon_prod WITH 'B'
RETURN
ENDPROC && mon_dpr

***************************************************************
* Naziv:	shw_expimp
* Poziv:	shw_expimp()
* Namjena:	Prikaz log tablice transfera podataka (Basler-Uniqa)
*			(akcija nad view-om V_M_EXPIMP)
* Autor:	Marko Bekafigo
* Datum:	31.07.2014.
***************************************************************
PROCEDURE shw_expimp
	PARAMETERS pid
	PRIVATE ssel, xok, xdt, gridname, datafstring
	ssel = SELECT()
	*
	pid = IIF(PCOUNT()=0, id, pid)
	*
	CREATE CURSOR ccexpimp (ind N(1), id N(12), red N(6), tip_poruke C(1), poruka C(254))
	*
	xdt = qReportsObject.shw_expimp(pid)
	loDotNetBridge.datasettocursors(xdt)
	*
	SELECT ccexpimp
	APPEND FROM DBF('ccExpimp_log')
	GO TOP
	*
	datafstring = 'shw_expimp'
	DO FORM shw_expimp NAME &datafstring
	*
	USE IN ccExpimp_log
	*
	SELECT (ssel)
	RETURN xdt
ENDPROC && shw_expimp

***************************************************************
* Naziv:	Export ORYX asistencija u xml
* Poziv:	EXP_ORYX()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	19.08.2014
***************************************************************
PROCEDURE exp_oryx
	PRIVATE xputanja, pfile
	xputanja=ALLTRIM(unx_not) + ALLTRIM(muser)
	pfile = unx_tmp+'export_oryx_log.txt'
	*
	SpremiLog(pfile,'*******************************************************')
	SpremiLog(pfile, '*')
	SpremiLog(pfile, 'POKRENUT EXPORT PODATAKA ZA ORYX ASISTENCIJU '+ DTOC(DATE())+' U '+TIME())
	IF qInsuranceObject.ExportORYX(xputanja)
		SpremiLog(pfile,'KREIRANA JE DATOTEKA NA LOKACIJI '+xputanja+'.')
	ELSE
		SpremiLog(pfile,'DATOTEKA NIJE KREIRANA.')
	ENDIF
	SpremiLog(pfile, 'ZAVRäETAK U '+ DTOC(DATE())+' '+TIME()+'')
	SpremiLog(pfile, '*')
	RETURN
ENDPROC && exp_oryx

***************************************************************
* Naziv:	Automatski dnevni export ORYX asistencija u xml za Kvig
* Poziv:	ORYX_EXPD()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	24.12.2014
***************************************************************
PROCEDURE oryx_expd
	DO oryx_exp WITH 'D', 1
	RETURN
ENDPROC && oryx_expd

***************************************************************
* Naziv:	Automatski mjeseËni export ORYX asistencija u xml za Kvig
* Poziv:	ORYX_EXPM()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	24.12.2014
***************************************************************
PROCEDURE oryx_expm
	DO oryx_exp WITH 'M', 1
	RETURN
ENDPROC && oryx_expm

***************************************************************
* Naziv:	Dnevni/MjeseËni export ORYX asistencija u xml za Kvig
* Poziv:	ORYX_EXP('D') ili ORYX_EXP('M'), opcionalni parametar pauto 0 ili 1
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	24.12.2014
***************************************************************
PROCEDURE oryx_exp
PARAMETERS psto, pauto
	PRIVATE xputanja, God, GDo, xauto
	upd_env('', IIF(xfqmat_kva, 'KVIG_AUTOASIS_RIZICI', 'AUTOASIS_RIZICI'))
	xputanja=ALLTRIM(pgcenv('ORYX_PUT'))
	GOd=DATE(YEAR(GOMONTH(DATE(),-1)),MONTH(DATE()),1)
	GDo=DATE()-DAY(DATE())+32-(DAY(DATE()-DAY(DATE())+32))
	IF PCOUNT()=1
		xauto=0
	ELSE
		xauto=pauto
	ENDIF
	*
	IF psto='D'
		fill_asistexp()
		qInsuranceObject.ExportORYXKvig('D', xputanja)
	ELSE
		IF xauto = 0
			PGC_RET2VAL('Export ORYX asistencije u xml','Period od','Period do', 'GOd', 'GDo')
		ELSE
			GOd=GOd-VAL(ALLTRIM(STRTRAN(pgcenv('DAT_OD'),'\')))
			GDo=GDo-VAL(ALLTRIM(STRTRAN(pgcenv('DAT_DO'),'\')))
		ENDIF
		IF !EMPTY(God) AND !EMPTY(GDo)
			qInsuranceObject.ExportORYXKvig('M', xputanja, God, GDo)
		ENDIF
	ENDIF
	RETURN
ENDPROC && oryx_exp

***************************************************************
* Naziv:	Automatski import ORYX asistencija za Kvig
* Poziv:	ORYX_IMPA()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	14.09.2015
***************************************************************
PROCEDURE oryx_impa
	DO oryx_imp WITH 1
	RETURN
ENDPROC && oryx_impa

***************************************************************
* Naziv:	RuËni import ORYX asistencija za Kvig
* Poziv:	ORYX_IMPR()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	14.09.2015
***************************************************************
PROCEDURE oryx_impr
	DO oryx_imp WITH 0
	RETURN
ENDPROC && oryx_impr

***************************************************************
* Naziv:	MjeseËni import ORYX asistencija u Kvig
* Poziv:	ORYX_IMP(0) ili ORYX_IMP(1)
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	12.01.2015
* Dorade:	28.01.2015. dodano premjeötanje datoteke zavisno o uspjeönosti importa
*			14.09.2015. funkcija parametrizirana za automatsko ili ruËno pokretanje
***************************************************************
PROCEDURE oryx_imp
PARAMETERS pauto
	PRIVATE ximp_putanja, ximp_put_done, ximp_put_err, ximp_file, xlist_file, xekstenzija, xauto, xok
	xok=.F.
	xlist_file=''
	xekstenzija='*.xml'
	IF PCOUNT()=0
		xauto=0
	ELSE
		xauto=pauto
	ENDIF
	upd_env('', IIF(xfqmat_kva, 'KVIG_AUTOASIS_RIZICI', 'AUTOASIS_RIZICI'))		&& upd_env('','KVIG_AUTOASIS_RIZICI')
	ximp_putanja=ALLTRIM(pgcenv('ORYX_IMP_PUT'))
	ximp_put_done=LEFT(PGCENV('ORYX_IMP_PUTD'),LEN(pgcenv('ORYX_IMP_PUTD'))-1)
	ximp_put_err=LEFT(PGCENV('ORYX_IMP_PUTE'),LEN(pgcenv('ORYX_IMP_PUTE'))-1)
	IF xauto=1 && automatski import (importira sve *.xml datoteke iz foldera)
		xlist_file=qInsuranceObject.ListaDatotekaUFolderu(ximp_putanja, xekstenzija)
		IF !EMPTY(xlist_file)
			xmltorecordset(xlist_file)
			SCAN ALL
				ximp_file=filename
				xok=qInsuranceObject.ImportORYXKvig(ximp_file)
				IF xok
					COPY FILE &ximp_file TO &ximp_put_done
					DELETE FILE &ximp_file RECYCLE
				ELSE
					COPY FILE &ximp_file TO &ximp_put_err
					DELETE FILE &ximp_file RECYCLE
				ENDIF
			ENDSCAN
		ENDIF
	ELSE && ruËni import (jedna po jedna datoteka)
		SET DEFAULT TO (ximp_putanja)
		ximp_file = GETFILE('xml','','',0,'Datoteka s ulaznim podacima')
		IF !EMPTY(ximp_file)
			xok=qInsuranceObject.ImportORYXKvig(ximp_file)
			IF xok
				COPY FILE &ximp_file TO &ximp_put_done
				DELETE FILE &ximp_file RECYCLE
			ELSE
				COPY FILE &ximp_file TO &ximp_put_err
				DELETE FILE &ximp_file RECYCLE
			ENDIF
		ENDIF
		SET DEFAULT TO (qpoc_dir)
	ENDIF
	RETURN
ENDPROC && oryx_imp

***************************************************************
* Naziv:	upd_eusanctions
* Poziv:	shw_expimp()
* Namjena:	Prikaz log tablice transfera podataka (Basler-Uniqa)
*			(akcija nad view-om V_M_EXPIMP)
* Autor:	Marko Bekafigo
* Datum:	31.07.2014.
***************************************************************
PROCEDURE upd_eusanctions
	PRIVATE xret
	xret = qInsuranceObject.UpdateEUSanctions()
	abc_message('EU SANKCIJSKA LISTA'+CHR(13)+CHR(13)+IIF(xret = -1, 'Neuspjeöno aûuriranje liste', 'Broj zapisa: '+ALLTRIM(STR(xret))))
	RETURN
ENDPROC && upd_eusanctions

***************************************************************
* Naziv:	Tisak faktura generiranih automatskim tarifiranjem
* Poziv:	TISF_AUTO_RP()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	15.10.2014.
***************************************************************
PROCEDURE tisf_auto_rp
	DO tisf_rp WITH 1
	RETURN
ENDPROC && tisf_auto_rp

***************************************************************
* Naziv:	upd_eusanctions_part
* Poziv:	upd_eusanctions_part()
* Namjena:	UpdateEUSanctionsPartner
* Autor:	Nikola MilkoviÊ
* Datum:	30.10.2014.
***************************************************************
PROCEDURE upd_eusanctions_part
	PRIVATE xret
	xret = qInsuranceObject.UpdateEUSanctionsPartner()
	RETURN
ENDPROC && upd_eusanctions_part


***************************************************************
* Naziv:	partstateu
* Poziv:	partstateu()
* Namjena:	Update statusa eu sankcija za partnera
* Autor:	Nikola MilkoviÊ
* Datum:	30.10.2014.
***************************************************************
PROCEDURE partstateu
	PRIVATE xret, XOnOff, xstat, xjmbg, xok, SSEL
	SSEL  = SELECT()
	xstat = ALLTRIM(Eu_sanct)
	xjmbg = jmbg
	xok = .F.
*!*		XOnOff=SET('CURSOR')
*!*		SET CURSOR ON
*!*		xstat=PGC_RETVAL('Unesite status partnera na sankcijskoj listi:',xstat)
*!*		SET CURSOR &XOnOff

	datafstring='partstateu'
	DO FORM partstateu NAME &datafstring

	IF !xok
		*ABC_MESSAGE('Unijeli ste krivi status partnera na sankcijskoj listi. MoguÊe vrijednosti su: ST, PT, NT')
		SELECT(SSEL)
		RETURN
	ENDIF
	ml_sql([update partneri set eu_sanct = ?xstat where jmbg = ?xjmbg ])
	SELECT(SSEL)
	RETURN
ENDPROC && upd_eusanctions_part


***************************************************************
* Naziv:	provj_eusanct
* Poziv:	provj_eusanct(jmbg)
* Namjena:	Update statusa eu sankcija za partnera
* Autor:	Nikola MilkoviÊ
* Datum:	30.10.2014.
***************************************************************
PROCEDURE maileusanct
	PARAMETERS pjmbg, pbody, precipient, psubject
	PRIVATE xok, ssel, xsubject, xbody, xserver, xuser, xpass, xfrom , xsender, xto, xreceiver, xreply, xcc, xformat
	SSEL  = SELECT()
	xok = .t.
	xserver = ALLTRIM(STRTRAN(PGCENV('MAILSERVER'),'\'))
	xuser = ALLTRIM(STRTRAN(PGCENV('MAILUSER'),'\'))
	xpass = ALLTRIM(STRTRAN(PGCENV('MAILPASSWORD'),'\'))
	xfrom = ALLTRIM(STRTRAN(PGCENV('MAILSENDER'),'\'))
	IF !EMPTY(xserver)
		IF PCOUNT() = 3 AND VARTYPE(precipient) == 'C'
			xmail_adr = precipient
		ELSE
			xmail_adr = STRTRAN(STRTRAN(PGCENV('EU_SANCTION_RECIPIENT'),'\'),';',',')
		ENDIF
		IF !EMPTY(xmail_adr)
			IF VARTYPE(psubject) = 'C' AND !EMPTY(psubject)
				xsubject = psubject
			ELSE
				xsubject = 'EU sankcijska lista - provjera'
			ENDIF

			IF PCOUNT() >= 2 AND VARTYPE(pbody) == 'C'
				xbody = pbody
			ELSE
				xbody = 'Unesena je stranka (ID:'+ pjmbg + ') za koju postoje sliËnosti sa osobama na EU sankcijskoj listi - Konsolidiranoj listi fiziËkih i pravnih osoba pod financijskim sankcijama EU. ' + ;
						+ CR_LF + ;
						'Provjeriti sa UNIQA Group Compliance i Uredom za SPNFT: ' + ;
						+ CR_LF + ;
						'Korisnik unosa stranke: ' + muser + ;
						+ CR_LF + ;
						'Datum unosa stranke: '  + DTOC(DATE())
			ENDIF

			xok = QCubisObject.send_mail(xfrom, xmail_adr, '', xsubject, xbody, '', xserver, xuser, xpass)
			IF !xok
				PRIVATE xmail_error_message
				xmail_error_message="Error Message: "+QCubisObject.Error+;
				+ CR_LF + ;
				"OD: "+xfrom+;
				+ CR_LF + ;
				"ZA: "+xmail_adr+;
				+ CR_LF + ;
				"Subject: "+xsubject+;
				+ CR_LF + ;
				"Poruka: "+xbody+;
				+ CR_LF + ;
				"Mail server podaci: "+xserver+";"+ xuser+";"+ xpass

				abc_message(xmail_error_message)
			ENDIF
		ENDIF
	ENDIF
	SELECT(SSEL)
	RETURN
ENDPROC && upd_eusanctions_part

***************************************************************
* Naziv:	import_tec_auto
* Poziv:	import_tec
* Namjena:	Import teËajne liste HNB bez poruke za schedule
* Autor:	Andreas CrnkoviÊ
* Datum:	21.06.2016.
***************************************************************
PROCEDURE import_tec_auto
	PRIVATE ssel, ximport_banka, ximport_datum, ximport_valuta
	ssel = SELECT()
	*
	upd_env('', 'IMPORT_TECAJ')
	ximport_banka = ALLTRIM(STRTRAN(PGCENV('IMPORT_BANKA'),'\'))
	ximport_valuta = ALLTRIM(STRTRAN(PGCENV('IMPORT_VALUTA'),'\'))
	ximport_datum = DATE()
	*
	qInsuranceObject.download_tecaj(ximport_banka, ximport_datum, ximport_valuta)
	*
	SELECT (ssel)
	RETURN
ENDPROC && import_tec

***************************************************************
* Naziv:	import_tec
* Poziv:	import_tec(pdatum) - 'D' (danas), 'S' (sutra)
* Namjena:	Import teËajne liste HNB
* Autor:	Marko Bekafigo
* Datum:	10.11.2014.
***************************************************************
PROCEDURE import_tec
	PARAMETERS pdatum
	PRIVATE ssel, ximport_banka, ximport_datum, ximport_valuta
	ssel = SELECT()
	*
	ximport_datum = IIF(PCOUNT()=1 AND pdatum='D', DATE(), DATE()+1)
	*
	upd_env('', 'IMPORT_TECAJ')
	ximport_banka = ALLTRIM(STRTRAN(PGCENV('IMPORT_BANKA'),'\'))
	ximport_valuta = ALLTRIM(STRTRAN(PGCENV('IMPORT_VALUTA'),'\'))
	*
	IF qInsuranceObject.download_tecaj(ximport_banka, ximport_datum, ximport_valuta)
		abc_message("TeËajna lista na '"+DTOC(ximport_datum)+"' uspjeöno importirana!")
	ELSE
		abc_message("Neuspjeöno uËitavanje teËajne liste na '"+DTOC(ximport_datum)+"' !!! "+qInsuranceObject.Error)
	ENDIF
	*
	SELECT (ssel)
	RETURN
ENDPROC && import_tec

***************************************************************
* Naziv:	Stari export podataka o policama PZO za Europe Assistance
* Poziv:	EXP_PZO1()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	03.12.2014
* Napomena: nakon testiranja nove funkcije ovu obrisati
***************************************************************
PROCEDURE exp_pzo1
DO pzo_cor WITH 1
RETURN
ENDPROC && exp_pzo1

***************************************************************
* Naziv:	Export podataka o policama PZO za Europe Assistance
* Poziv:	EXP_PZO()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	24.03.2015
***************************************************************
PROCEDURE exp_pzo
	PRIVATE xvar
	xvar=qInsuranceObject.ExportEuropAssist()
	RETURN
ENDPROC && exp_pzo


***************************************************************
* Naziv:	provj_opo
* Poziv:	provj_opo(broj)
* Namjena:	Provjera da li se trebala napraviti opomena po polici
* Autor:	Nikola MilkoviÊ
* Datum:	18.12.2014.
***************************************************************
PROCEDURE provj_opo
	PARAMETERS pbroj
	PRIVATE xok, ssel, xvrs_opom, XOnOff, XFUNC_PRO, xprod_banko, xdana
	SSEL  = SELECT()
	xvrs_opom = ''
	xprod_banko = STRTRAN(PGCENV('PROD_STRU_BANKO'), '\')
	XFUNC_PRO  = ''
	XOnOff=SET('CURSOR')
	SET CURSOR ON
	xvrs_opom=PGC_RETVAL('Unesite vrstu opomene za koju ûelite provjeriti policu:',xvrs_opom,'Ext(.F.,RetVal,"RetVal","vrs_opom","sifra")')
	SET CURSOR &XOnOff
	IF VARTYPE(xvrs_opom) == 'C' AND !EMPTY(ALLTRIM(xvrs_opom))
		ML_SQL([SELECT func_provj, dug_dana from vrs_opom where sifra = ?xvrs_opom INTO CURSOR TVRST])
		XFUNC_PRO = func_provj
		xdana = dug_dana
		USE IN TVRST
		IF !EMPTY (XFUNC_PRO)
			TRY
				DO &XFUNC_PRO WITH pbroj
			CATCH
				ABC_MESSAGE('Funkcija za provjeru je neispravna!')
			ENDTRY

		ELSE
			ABC_MESSAGE('Nije definirana funkcija za provjeru!')
		ENDIF
	ENDIF

	SELECT(SSEL)
	RETURN
ENDPROC

PROCEDURE provj1_opo
PARAMETERS pbroj
PRIVATE xok, ssel, xdatum
	SSEL  = SELECT()
	xdatum = DATE()-xdana
	xporuka = qinsuranceobject.provj_opo(pbroj, xprod_banko, xdatum, .F., .F., '')
	IF !EMPTY(xporuka)
		abc_message(xporuka)
	ELSE
		abc_message('Po zadanim uvjetima trebala se napraviti opomena.')
	ENDIF
	SELECT(SSEL)
	RETURN
ENDPROC

PROCEDURE provj1_opovip
PARAMETERS pbroj
PRIVATE xok, ssel, xprod_banko , xdatum
	SSEL  = SELECT()
	xdatum = DATE()-xdana
	xporuka = qinsuranceobject.provj_opo(pbroj, xprod_banko, xdatum, .F., .T., '')
	IF !EMPTY(xporuka)
		abc_message(xporuka)
	ELSE
		abc_message('Po zadanim uvjetima trebala se napraviti opomena.')
	ENDIF
	SELECT(SSEL)
	RETURN xporuka
ENDPROC

PROCEDURE provj1_opobanko
PARAMETERS pbroj
PRIVATE xok, ssel, xprod_banko , xdatum
	SSEL  = SELECT()
	xdatum = DATE()-xdana
	xporuka = qinsuranceobject.provj_opo(pbroj, xprod_banko, xdatum, .t., .f., '')
	IF !EMPTY(xporuka)
		abc_message(xporuka)
	ELSE
		abc_message('Po zadanim uvjetima trebala se napraviti opomena.')
	ENDIF
	SELECT(SSEL)
	RETURN
ENDPROC

PROCEDURE provj2_vrs
PARAMETERS pbroj
PRIVATE xok, ssel, xprod_banko, xdatum
	SSEL  = SELECT()
	xdatum = DATE()-xdana
	xporuka = qinsuranceobject.provj_opo(pbroj, xprod_banko, xdatum, .F., .F., xvrs_opom)
	IF !EMPTY(xporuka)
		abc_message(xporuka)
	ELSE
		abc_message('Po zadanim uvjetima trebala se napraviti opomena.')
	ENDIF
	SELECT(SSEL)
	RETURN
ENDPROC

PROCEDURE provj2_vrsbanko
PARAMETERS pbroj
PRIVATE xok, ssel, xprod_banko , xdatum
	SSEL  = SELECT()
	xdatum = DATE()-xdana
	xporuka = qinsuranceobject.provj_opo(pbroj, xprod_banko, xdatum, .T., .F., xvrs_opom)
	IF !EMPTY(xporuka)
		abc_message(xporuka)
	ELSE
		abc_message('Po zadanim uvjetima trebala se napraviti opomena.')
	ENDIF
	SELECT(SSEL)
	RETURN
ENDPROC

***************************************************************
* Naziv:	opo_po_pol
* Poziv:	opo_po_pol(broj)
* Namjena:	Izrada opomene po polici bez uvjeta
* Autor:	Nikola MilkoviÊ
* Datum:	18.12.2014.
***************************************************************
PROCEDURE opo_po_pol
PARAMETERS PBROJ
	DO genopo_po_pol WITH pbroj IN opo_gen
ENDPROC

***************************************************************
* Naziv:	Punjenje tablice ASIST_EXP podacima o policama s ugovorenim rizikom ORYX
* Poziv:	fill_asistexp()
* Opis:		()
* Autor:	Zrinko PuljiÊ
* Datum:	22.12.2014
* Dorada:	07.07.2020 Maja - novi parametri za poziv izmijenjene dll funkcije
*			29.09.2020 Marko - Wiener akcija (IW poklon asistencija)
***************************************************************
PROCEDURE fill_asistexp
	PRIVATE pfile, xod, xdo, xdana_unazad, xxml, xasist_proizv, xasist_proizv_im, xasist_proizv_dz, xxlm_akc, xasist_proizv_akc, xakcija_od
	*
	IF upd_env('', IIF(xfqmat_kva, 'KVIG_AUTOASIS_RIZICI', 'AUTOASIS_RIZICI'))
		qsif_riz=ALLTRIM(STRTRAN(PGCENV('SIF_RIZ'),'\'))
		qsif_riz=STRTRAN(qsif_riz,';',',')
		*
		xasist_proizv=ALLTRIM(STRTRAN(PGCENV('ASIST_PROIZV'),'\'))
		xasist_proizv=STRTRAN(xasist_proizv,';',',')
		*
		xasist_proizv_im=ALLTRIM(STRTRAN(PGCENV('ASIST_PROIZV_IM'),'\'))
		xasist_proizv_im=STRTRAN(xasist_proizv_im,';',',')
		*
		xasist_proizv_dz=ALLTRIM(STRTRAN(PGCENV('ASIST_PROIZV_DZ'),'\'))
		xasist_proizv_dz=STRTRAN(xasist_proizv_dz,';',',')
		*
		xasist_proizv_akc=ALLTRIM(STRTRAN(PGCENV('ASIST_PROIZV_AKC'),'\'))
		xasist_proizv_akc=STRTRAN(xasist_proizv_akc,';',',')
		*
		xdana_unazad=VAL(STRTRAN(PGCENV('DANA_UNAZAD'),'\'))
		qdana_unazad=IIF(xdana_unazad>0, xdana_unazad, 5)
		*
	ELSE
		* abc_message('Niste definirali potrebne parametre u MEGALINE.INI datoteci!')
		SpremiLog(pfile, 'Niste definirali potrebne parametre u MEGALINE.INI datoteci!')
		RETURN
	ENDIF
	*
	xod = DATE()-qdana_unazad
	xdo = DATE()
	STORE 0 TO xxml, xxlm_akc
	*
	pfile = unx_tmp+'punjenje_asist_exp_log.txt'
	*
	SpremiLog(pfile,'*******************************************************')
	SpremiLog(pfile, '*')
	SpremiLog(pfile, 'POKRENUTO PUNJENJE PODATAKA U TABLICU ASIST_EXP '+ DTOC(DATE())+' U '+TIME())
	*
	xxml = qInsuranceObject.PunjenjeTabliceAsistExp(xod, xdo, qsif_riz, xasist_proizv, xasist_proizv_im, xasist_proizv_dz)

	IF xfqmat_kva AND pgcenv('ASIST_AKCIJA','ON')
		xakcija_od = CTOD(ALLT(STRTRAN(pgcenv('ASIST_DATUM_AKC'),'\')))
		IF !EMPTY(xakcija_od)
			xakcija_od= MAX(xod, xakcija_od)
			xxlm_akc = qInsuranceObject.PunjenjeTabliceAsistExpAkcijaW(xakcija_od, xdo, xasist_proizv_akc)
		ENDIF
	ENDIF 
	
	IF (xxml+xxlm_akc)>0 
		SpremiLog(pfile,'Punjenje podataka zavröeno. Importirano je ' +ALLTRIM(STR(xxml+xxlm_akc)) + ' novih polica.')
	ELSE
		SpremiLog(pfile,'Punjenje podataka zavröeno. Nije importirana niti jedna polica.')
	ENDIF
	SpremiLog(pfile, 'ZAVRäETAK U '+ DTOC(DATE())+' '+TIME()+'')
	SpremiLog(pfile, '*')
	*
	* Marko (20.01.2015) - Automatsko generiranje öteta po policama sa ugovorenim rizikom ORYX Asistencija
	SpremiLog(pfile, 'PO»ETAK Automatsko generiranje öteta po policama sa ugovorenim rizikom ORYX Asistencija')
	fill_asist_stet((qsif_riz))
	SpremiLog(pfile, 'ZAVRäETAK Automatsko generiranje öteta po policama sa ugovorenim rizikom ORYX Asistencija')
	*
	RETURN
ENDPROC && fill_asistexp

***************************************************************
* Naziv:	Automatsko generiranje öteta po policama sa ugovorenim rizikom ORYX Asistencija
* Poziv:	fill_asist_stet()
* Opis:		()
* Autor:	Marko Bekafigo
* Datum:	20.01.2015
* Izmjena:	Marko (11.11.2015) - storno likvidacija öteta za stornirane police (vrsta_cl='S')
*           Marko (15.02.2017) - dorada za police koje imaju dvije asistencije (ORYX i AUTOPASS)
*			Marko (04.05.2017) - za Izvor vratio datum likvidacije ötete na POC_CLAN umjesto QDATEVAR
***************************************************************
PROCEDURE fill_asist_stet
	PARAMETERS psif_riz
	PRIVATE ssel, xrbr_ao, xrbr_im, xrbr_storno, xasist_sdog, xasist_ispl, xasist_vrsta, xasist_grupa, xasist_sd, ;
		xknj, xsteta, xpgcvars, xserial, xproizvod, xbroj_pol, xdatum, xgodina, xvaz_od, xvaz_do, xstet_oib, ;
		xstet_naz, xstet_adr, xstet_mje, xreg_ozn, xmarka, xser, xtab_pol, xiz_meni, xizn_likv, xvrsta_osig, ;
		xasist_ste, xasist_kor, xasist_osnov, xasist_knj, xasist_pro, xasist_izn_01, xasist_izn_02, xasist_izn_03, ;
		xasist_izn_07, xasist_izn_09, xasi_knj_im, xasi_pro_im, xasi_izn_im_01, xasi_izn_im_02, xosnov_fld, ;
		xknjiga_poc, xprom_stat, xtab_knjst, xstat_id, xpaket, xugo_id, xasist_izn_11, xasist_izn_12, ;
		xasi_knj_dz, xasi_pro_dz, xasi_izn_dz_01, xasi_izn_dz_02, xasi_izn_dz_03, xrbr_dz, xval
	ssel = SELECT()
	*
	STORE 0 TO xrbr_ao, xrbr_im, xrbr_dz, xrbr_storno
	xknjiga_poc = DATE(2016,1,1)
	xprom_stat = 'ST'
	xval = IIF(VARTYPE(xlikv_osn_val)='C' AND !EMPTY(xlikv_osn_val), xlikv_osn_val, qosn_val)
	*
	xiz_meni = .F.
	IF PCOUNT()=0
		xiz_meni = .T.
		upd_env('', IIF(xfqmat_kva, 'KVIG_AUTOASIS_RIZICI', 'AUTOASIS_RIZICI'))		&& UPD_ENV('','KVIG_AUTOASIS_RIZICI')
		psif_riz = STRTRAN(ALLTRIM(STRTRAN(PGCENV('SIF_RIZ'),'\')), ';', ',')
	ENDIF
	*
	xasist_ste = PGCENV('ASIST_STETE','ON')
	xasist_kor = ALLTRIM(STRTRAN(PGCENV('ASIST_KOR_LIKV'),'\'))
	xasist_osnov = ALLTRIM(STRTRAN(PGCENV('ASIST_OSNOV'),'\'))
	xosnov_fld = ret_ext(xasist_osnov, 'osnov_is',,'polje_li','osnov')
	*
	xasist_knj = ALLTRIM(STRTRAN(PGCENV('ASIST_KNJIGA'),'\'))
	xasist_pro = ALLTRIM(STRTRAN(PGCENV('ASIST_PROIZV'),'\'))
	xasist_izn_01 = VAL(ALLTRIM(STRTRAN(STRTRAN(PGCENV('ASIST_IZN_01'),'\'),'.',',')))
	xasist_izn_02 = VAL(ALLTRIM(STRTRAN(STRTRAN(PGCENV('ASIST_IZN_02'),'\'),'.',',')))
	xasist_izn_03 = VAL(ALLTRIM(STRTRAN(STRTRAN(PGCENV('ASIST_IZN_03'),'\'),'.',',')))
	xasist_izn_07 = VAL(ALLTRIM(STRTRAN(STRTRAN(PGCENV('ASIST_IZN_180507'),'\'),'.',',')))
	xasist_izn_09 = VAL(ALLTRIM(STRTRAN(STRTRAN(PGCENV('ASIST_IZN_180509'),'\'),'.',',')))
	xasist_izn_11 = VAL(ALLTRIM(STRTRAN(STRTRAN(PGCENV('ASIST_IZN_180511'),'\'),'.',',')))
	xasist_izn_12 = VAL(ALLTRIM(STRTRAN(STRTRAN(PGCENV('ASIST_IZN_180512'),'\'),'.',',')))
	*
	xasi_knj_im = ALLTRIM(STRTRAN(PGCENV('ASIST_KNJIGA_IM'),'\'))
	xasi_pro_im = ALLTRIM(STRTRAN(PGCENV('ASIST_PROIZV_IM'),'\'))
	xasi_izn_im_01 = VAL(ALLTRIM(STRTRAN(STRTRAN(PGCENV('ASIST_IZN_IM_01'),'\'),'.',',')))
	xasi_izn_im_02 = VAL(ALLTRIM(STRTRAN(STRTRAN(PGCENV('ASIST_IZN_IM_02'),'\'),'.',',')))
	*
	xasi_knj_dz = ALLTRIM(STRTRAN(PGCENV('ASIST_KNJIGA_DZ'),'\'))
	xasi_pro_dz = ALLTRIM(STRTRAN(PGCENV('ASIST_PROIZV_DZ'),'\'))
	xasi_izn_dz_01 = VAL(ALLTRIM(STRTRAN(STRTRAN(PGCENV('ASIST_IZN_DZ_01'),'\'),'.',',')))
	xasi_izn_dz_02 = VAL(ALLTRIM(STRTRAN(STRTRAN(PGCENV('ASIST_IZN_DZ_02'),'\'),'.',',')))
	xasi_izn_dz_03 = VAL(ALLTRIM(STRTRAN(STRTRAN(PGCENV('ASIST_IZN_DZ_03'),'\'),'.',',')))
	*
	IF !xasist_ste OR EMPTY(xasist_kor) OR (EMPTY(xasist_knj) AND EMPTY(xasi_knj_im) AND EMPTY(xasi_knj_dz))
		IF xiz_meni			&& poziv iz izbornika
			abc_message(pgc_dlang('Niste definirali potrebne parametre u MEGALINE.INI datoteci!'))
		ENDIF
		RETURN
	ENDIF
	*
	* Podaci o korisniku likvidacije
	STORE '' TO xstet_oib, xstet_naz, xstet_adr, xstet_mje
	ml_sql([SELECT p.jmbg, p.pdv_br AS oib, p.naziv, p.adresa, p.mjesto ]+;
		[ FROM kor_likv k, partneri p ]+;
		[ WHERE k.sif_part=p.jmbg AND k.sifra=?xasist_kor ]+;
		[ INTO CURSOR tmppartner])
	IF _TALLY>0
		xstet_oib = oib
		xstet_naz = naziv
		xstet_adr = adresa
		xstet_mje = mjesto
	ENDIF
	USE IN tmppartner
	*
	* Obrada polica (tablica asist_exp) - vrsta_cl='N'
	ml_sql([SELECT * FROM asist_exp ]+;
		[ WHERE steta=' ' AND proizvod<>' ' AND polica<>' ' AND vrsta_cl<>'S' ]+;
		[ ORDER BY proizvod, polica ]+;
		[ INTO CURSOR tmpasist_exp])
	SCAN ALL
		IF !(EMPTY(proizvod) OR EMPTY(polica) OR EMPTY(poc_clan))
			xserial = serial
			xproizvod = proizvod
			xbroj_pol = polica
			xdatum = IIF(xfqmat_izv, poc_clan, qdatevar)

			* Marko (15.02.2017) - problemi sa generiranjem öteta zbog viöe rizika asistencije na jednoj polici
			xpaket = ALLTRIM(paket)
			DO CASE
				CASE xpaket=='1'
					psif_riz = [180401','180403','180404','180501','180503','180504','180505','180506','180508]
				CASE xpaket=='4'
					psif_riz = [180402','180502]
				CASE xpaket=='5'
					psif_riz = [09040401','09040402','180503]
				CASE xpaket=='6'
					psif_riz = IIF(xfqmat_izv, [189901], [180507])
				CASE xpaket=='7'
					psif_riz = [180509]
*!*				CASE xpaket=='8'					&& Marko, Maja (27.01.2020) - CASE za paket '8' do sada nije postojao i nitko se nije ûalio
*!*					psif_riz = [180510]
				CASE xpaket=='9'
					psif_riz = [180511','180512]
				CASE xpaket=='11'
					psif_riz = [02990001]
				CASE xpaket=='12'
					psif_riz = [02990002]
				CASE xpaket=='13'
					psif_riz = [02990003]
				OTHERWISE
					psif_riz = [.]
			ENDCASE

			* Marko (15.02.2017) - öteta se kreira za godinu kada se radi obrada
			xgodina = RIGHT(STR(YEAR(DATE()),4),2)		&& RIGHT(STR(YEAR(xdatum),4),2)

			STORE {} TO xvaz_od, xvaz_do
			STORE '' TO xreg_ozn, xmarka, xugo_id
			STORE 0 TO xizn_likv, xasist_sdog
			*
			DO CASE
				CASE xproizvod $ xasist_pro		&& Wiener AO
					xknj = xasist_knj
				CASE xproizvod $ xasi_pro_im	&& Wiener IM, Izvor IM
					xknj = xasi_knj_im
				CASE xproizvod $ xasi_pro_dz	&& Wiener DI, DD
					xknj = xasi_knj_dz
				OTHERWISE
					xknj = '.'
			ENDCASE
			STORE '' TO xtab_knjst, xasist_ispl, xasist_vrsta, xasist_grupa, xasist_sd
			*
			ml_sql([SELECT * FROM knjige WHERE knjiga_os=?xknj INTO CURSOR tmpknj])
			IF _TALLY=1
				xtab_knjst = ALLTRIM(dat_prij)
				xasist_ispl = ALLTRIM(tbl_ispl)
				xasist_vrsta = ALLTRIM(skupina)
				xasist_grupa = ALLTRIM(grupa_os)
				xasist_sd = ALLTRIM(tbl_stet)
				*
				IF !(EMPTY(xasist_ispl) OR EMPTY(xasist_vrsta) OR EMPTY(xasist_grupa))
					*
					* provjera rizika
					ml_sql([SELECT * FROM st_pol ]+;
						[ WHERE proizvod=?xproizvod AND broj=?xbroj_pol AND stat_id IN (']+psif_riz+[') ]+;
						[ INTO CURSOR tmpst_pol]) && koristiti varijablu iz megaline.ini
					SCAN ALL
						DO CASE
							CASE xfqmat_izv
								DO CASE
									CASE stat_id = '180401' OR stat_id = '180501'
										xizn_likv = xasist_izn_01	&& 60 kn
									CASE stat_id = '180402' OR stat_id = '180502'
										xizn_likv = xasist_izn_02	&& 70 kn
									CASE stat_id = '180503'
										xizn_likv = xasist_izn_03	&& 110 kn
									CASE stat_id = '189901'
										xizn_likv = xasi_izn_im_01	&& 48 kn
								ENDCASE
							CASE xfqmat_kva
								DO CASE
									CASE INLIST(stat_id, '180401', '180403', '180404', '180501', '180504', '180505', '180506', '180508')
										xizn_likv = xasist_izn_01
									CASE INLIST(stat_id, '180402', '180502')
										xizn_likv = xasist_izn_02
									CASE (stat_id = '180507')
										xizn_likv = xasist_izn_07
									CASE (stat_id = '180509')
										xizn_likv = xasist_izn_09
*!*									CASE (stat_id = '180509')		&& Marko, Maja (27.01.2020) - CASE za paket '8' do sada nije postojao i nitko se nije ûalio
*!*										xizn_likv = xasist_izn_09
									CASE (stat_id = '180511')
										xizn_likv = xasist_izn_11
									CASE (stat_id = '180512')
										xizn_likv = xasist_izn_12
									CASE (stat_id = '09040401')
										xizn_likv = xasi_izn_im_01
									CASE (stat_id = '09040402')
										xizn_likv = xasi_izn_im_02
*!*									CASE INLIST(stat_id, '180505', '180506', '180508')
*!*										xizn_likv = xasist_izn_01
									CASE (stat_id = '02990001')
										xizn_likv = xasi_izn_dz_01
									CASE (stat_id = '02990002')
										xizn_likv = xasi_izn_dz_02
									CASE (stat_id = '02990003')
										xizn_likv = xasi_izn_dz_03
								ENDCASE
							OTHERWISE
								DO CASE
									CASE INLIST(stat_id, '180401', '180403', '180404', '180501', '180504')
										xizn_likv = xasist_izn_01
									CASE INLIST(stat_id, '180402', '180502')
										xizn_likv = xasist_izn_02
									CASE stat_id = '09040401'
										xizn_likv = xasi_izn_im_01
									CASE stat_id = '09040402'
										xizn_likv = xasi_izn_im_02
								ENDCASE
						ENDCASE
						xstat_id = stat_id
						*
						* INSERT_STETE
						*
						IF xizn_likv>0

							* Generiranje broja öteta i ötetnog dogaaja
							gbroj = getbrst(xgodina, xknj, xtab_knjst)

							add_sd(xasist_grupa, xasist_sd, xasist_sdog, '.', '.', (xdatum), '.', '.', '.', '.', (qempty_date), (xreg_ozn), 0, '.', (gbroj+ALLTRIM(xknj)))

							* Marko (15.02.2017) - podaci sa police AO
							xtab_pol = ret_ext(xproizvod, 'skup_os', , 'tab_pol', 'sifra')
							IF !EMPTY(xtab_pol) AND ml_sql([SELECT * FROM ]+xtab_pol+[ WHERE broj=?xbroj_pol INTO CURSOR tmppolica])
								IF _TALLY>0
									xvaz_od = poc_dat
									xvaz_do = ist_dat
									xreg_ozn = IIF(VARTYPE(reg_ozn)='C', reg_ozn, '')
									xmarka = IIF(VARTYPE(marka)='C', marka, '')
									IF xfqmat_izv
										xugo_id = ugo_id
									ENDIF
								ENDIF
								USE IN tmppolica
							ENDIF

							IF xproizvod $ xasist_pro

								* Marko (15.02.2017) - xstat_id umjesto xasist_vrsta, qdatevar umjesto xdatum
								* Marko (04.05.2017) - za Izvor poc_clan umjesto qdatevar varijabla xdatum
								IF xfqmat_izv
									* Maja (12.12.2017) - za Izvor u jmbg_ost se upisuje OIB ORYX-a, a u jmbg_stet ugo_id s police
									ml_sql([INSERT INTO ]+xtab_knjst+[ ]+;
										[(knjiga, broj, dat_pri, ste_dog, polica1, vrsta_osig, vaz_od1, vazenje1, jmbg_ost, jmbg_stet, ]+;
										[ vlasnik1, adresa1, mjesto1, reg_ozn1, marka1, status, dat_likv, ]+xosnov_fld +[) ]+;
										[ VALUES (?xknj, ?gbroj, ?qdatevar, ?xasist_sdog, ?xbroj_pol, ?xstat_id, ?xvaz_od, ]+;
										[ ?xvaz_do, ?xstet_oib, ?xugo_id, ?xstet_naz, ?xstet_adr, ?xstet_mje, ?xreg_ozn, ?xmarka, 'LI', ]+;
										[ ?xdatum, ?xizn_likv)])
								ELSE
									ml_sql([INSERT INTO ]+xtab_knjst+[ ]+;
										[(knjiga, broj, dat_pri, ste_dog, polica1, vrsta_osig, vaz_od1, vazenje1, jmbg_stet, ]+;
										[ vlasnik1, adresa1, mjesto1, reg_ozn1, marka1, status, dat_likv, ]+xosnov_fld +[) ]+;
										[ VALUES (?xknj, ?gbroj, ?qdatevar, ?xasist_sdog, ?xbroj_pol, ?xstat_id, ?xvaz_od, ]+;
										[ ?xvaz_do, ?xstet_oib, ?xstet_naz, ?xstet_adr, ?xstet_mje, ?xreg_ozn, ?xmarka, 'LI', ]+;
										[ ?xdatum, ?xizn_likv)])
								ENDIF
								xrbr_ao = xrbr_ao + 1
							ELSE
								IF xproizvod $ xasi_pro_im 
									* Marko (15.02.2017) - xstat_id umjesto xasist_vrsta, qdatevar umjesto xdatum
									* Marko (04.05.2017) - za Izvor poc_clan umjesto qdatevar varijabla xdatum
									ml_sql([INSERT INTO ]+xtab_knjst+[ ]+;
										[(knjiga, broj, dat_pri, ste_dog, polica1, vrsta_osig, vaz_od1, vazenje1, mat_broj, ]+;
										[ vlasnik1, adresa1, mjesto1, status, dat_likv, ]+xosnov_fld +[) VALUES (?xknj, ?gbroj, ]+;
										[ ?qdatevar, ?xasist_sdog, ?xbroj_pol, ?xstat_id, ?xvaz_od, ?xvaz_do, ?xstet_oib, ]+;
										[ ?xstet_naz, ?xstet_adr, ?xstet_mje, 'LI', ?xdatum, ?xizn_likv)])
									xrbr_im = xrbr_im + 1
								ELSE							&& xproizvod $ xasi_pro_dz
									ml_sql([INSERT INTO ]+xtab_knjst+[ ]+;
										[(knjiga, broj, dat_pri, ste_dog, polica1, vrsta_osig, vaz_od1, vazenje1, mat_broj, ]+;
										[ vlasnik1, adresa1, mjesto1, status, dat_likv, ]+xosnov_fld +[) VALUES (?xknj, ?gbroj, ]+;
										[ ?qdatevar, ?xasist_sdog, ?xbroj_pol, ?xstat_id, ?xvaz_od, ?xvaz_do, ?xstet_oib, ]+;
										[ ?xstet_naz, ?xstet_adr, ?xstet_mje, 'LI', ?xdatum, ?xizn_likv)])
									xrbr_dz = xrbr_dz + 1
								ENDIF 
							ENDIF
							*
							* INSERT LIKVIDACIJE
							*
							xser = GET_SER()

							* Marko (15.02.2017) - qdatevar umjesto xdatum
							* Marko (04.05.2017) - za Izvor poc_clan umjesto qdatevar varijabla xdatum
							* Marko (01.09.2020) - aûuriranje valute, valutnog iznosa i teËaja likvidacije
							ml_sql([INSERT INTO ]+xasist_ispl+[ ]+;
								[(serial, knjiga, br_prijave, sifra_os, opis, sif_kor, stat_l, dat_likv, izn_likv, sredstvo, ]+;
								[ sif_val, val_izn, tec_likv) VALUES (?xser, ?xknj, ?gbroj, ?xasist_osnov, 'Asistencija', ]+;
								[ ?xasist_kor, 'K', ?xdatum, ?xizn_likv, 'V', ?xval, ?xizn_likv, 1)])
							*
							* Aûuriranje ASIST_EXP
							ml_sql([UPDATE asist_exp SET steta=(TRIM(?xknj)+'/'+?gbroj )WHERE serial=?xserial])
							*
						ENDIF
					ENDSCAN
					USE IN tmpst_pol
				ENDIF
			ENDIF
			USE IN tmpknj
		ENDIF
		*
		SELECT tmpasist_exp
	ENDSCAN
	*
	* Obrada polica (tablica asist_exp) - vrsta_cl='S' (storno likvidacija öteta za stornirane police - vrsta_cl='S')
	ml_sql([SELECT t1.*, t2.steta AS steta_n ]+;
		[ FROM asist_exp t1, asist_exp t2 ]+;
		[ WHERE t1.proizvod=t2.proizvod AND t1.polica=t2.polica AND t1.steta=' ' AND t1.vrsta_cl='S' AND t2.vrsta_cl='N' AND t2.steta<>' ' ]+;
		[ ORDER BY t1.proizvod, t1.polica ]+;
		[ INTO CURSOR tmpasist_exp])
	*
	SCAN ALL
		IF !(EMPTY(proizvod) OR EMPTY(polica))
			xserial = serial
			xdatum = dat_stor
			xknj = LEFT(steta_n, AT('/',steta_n)-1)
			xsteta = SUBSTR(steta_n, AT('/',steta_n)+1)
			xasist_ispl = ALLTRIM(ret_ext(xknj, 'knjige', ' ', 'tbl_ispl', 'knjiga_os'))
			*
			* Maja (09.05.2019) - punjenje xtab_knjst i xasist_ispl
			STORE '' TO xtab_knjst, xasist_ispl
			ml_sql([SELECT * FROM knjige WHERE knjiga_os=?xknj INTO CURSOR tmpknj1])
			IF _TALLY=1
				xtab_knjst = ALLTRIM(dat_prij)
				xasist_ispl = ALLTRIM(tbl_ispl)
				*
				ml_sql([SELECT SUM(izn_likv) AS izn_likv FROM ]+xasist_ispl+[ ]+;
					[ WHERE knjiga=?xknj AND br_prijave=?xsteta ]+;
					[ INTO CURSOR tmplikv])
				*
				IF _TALLY>0 AND izn_likv>0
					xizn_likv = -izn_likv
					xser = GET_SER()
					*
					* Insert storno likvidacije
					* Marko (10.12.2015) - izmjena datuma likvidacije (date() umjesto datuma storna iz asist_exp) - (Sonja)
					* Marko (01.09.2020) - aûuriranje valute, valutnog iznosa i teËaja likvidacije
					ml_sql([INSERT INTO ]+xasist_ispl+[ ]+;
						[(serial, knjiga, br_prijave, sifra_os, opis, sif_kor, stat_l, dat_likv, izn_likv, sredstvo, ]+;
						[ sif_val, val_izn, tec_likv) VALUES (?xser, ?xknj, ?xsteta, ?xasist_osnov, 'Asistencija - storno', ]+;
						[ ?xasist_kor, 'K', ?qdatevar, ?xizn_likv, 'V', ?xval, ?xizn_likv, 1)])
					*
					xrbr_storno = xrbr_storno + 1
					*
					* Aûuriranje ASIST_EXP
					ml_sql([UPDATE asist_exp SET steta=(TRIM(?xknj)+'/'+?xsteta) WHERE serial=?xserial])
					*
					* Aûuriranje prijave ötete
					* Marko (15.02.2017) - qdatevar umjesto xdatum
					ml_sql([UPDATE ]+xtab_knjst+[ ]+;
						[ SET dat_likv=?qdatevar, ]+xosnov_fld+[=]+xosnov_fld+[+?xizn_likv ]+;
						[ WHERE knjiga=?xknj AND broj=?xsteta])
				ENDIF
				USE IN tmplikv
			ENDIF
			USE IN tmpknj1
		ENDIF
		*
		SELECT tmpasist_exp
	ENDSCAN
	*
	IF xiz_meni			&& poziv iz izbornika
		abc_message([OBAVIJEST:]+CHR(13)+CHR(13)+;
			[Broj kreiranih öteta za police sa ugovorenom ORYX asistencijom]+CHR(13)+CHR(13)+;
			[MV: ]+ALLTRIM(STR(xrbr_ao,5))+CHR(13)+CHR(13)+;
			[IM: ]+ALLTRIM(STR(xrbr_im,5))+CHR(13)+CHR(13)+;
			[STORNO: ]+ALLTRIM(STR(xrbr_storno,5)))
	ENDIF
	*
	USE IN tmpasist_exp
	SELECT (ssel)
RETURN
ENDPROC && fill_asist_stet

***************************************************************
* Naziv:	Poziv update EU_SANCTIONS
* Poziv:	eu_sanct(.T.) ILI eu_sanct(.F.)
* Opis:		()
* Autor:	Nikola MilkoviÊ
* Datum:	04.02.2015
***************************************************************
PROCEDURE eusanct
PARAMETERS pporuka
	RETURN UN_SANCTIONS(pporuka,'E')
ENDPROC


***************************************************************
* Naziv:	Poziv update liste sankcija UN
* Poziv:	unsanct()
* Opis:		()
* Autor:	Nikola MilkoviÊ
* Datum:	07.10.2015
***************************************************************
PROCEDURE unsanct
	RETURN UN_SANCTIONS(.T.,'U')
ENDPROC

***************************************************************
* Naziv:	Poziv update liste sankcija UN
* Poziv:	unsanct1()
* Opis:		()
* Autor:	Nikola MilkoviÊ
* Datum:	07.10.2015
***************************************************************
PROCEDURE unsanct1
	RETURN UN_SANCTIONS(.f.,'U')
ENDPROC
***************************************************************
* Naziv:	Export DZO polica u excel radi izrade zdravstvenih kartica
* Poziv:	DZOKAR_EXP()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	20.02.2015
*			04.10.2016 Dodao metodu za WIENER(Zrinko)
***************************************************************
PROCEDURE dzokar_exp
	PROGRESS_CREATE('*** EXPORT DZO KARTICA ***')
	DO CASE
		CASE xfqmat_kva
		IF upd_env('', 'DZO_ISKAZNICE')
			PRIVATE xdana_unazad
			xdzo_exp=STRTRAN(PGCENV('DZO_EXP'),'\')
		ENDIF
		qInsuranceObject.ExportDZOKARTw()
	CASE xfqmat_uni
		qInsuranceObject.ExportDZOKARTu()
	CASE xfqmat_tri
		IF upd_env('', 'DZO_ISKAZNICE')
			PRIVATE xdana_unazad
			xdzo_exp=STRTRAN(PGCENV('DZO_EXP'),'\')
		ENDIF
		qInsuranceObject.ExportDZOKARTt()
	ENDCASE
	PROGRESS_DESTROY()
	RETURN
ENDPROC && dzokar_exp


***************************************************************
* Naziv:	Export DZO polica u excel radi izrade zdravstvenih kartica
* Poziv:	DZOKAR_EXDO()
* Opis:		()
* Autor:	Zrinko
* Datum:	21.11.2017
***************************************************************
PROCEDURE dzokar_exdo
	PRIVATE datafstring, xcjenik
	datafstring = 'dzokar_exdo'
	xcjenik = 'E001'
	*
	ml_sql([SELECT naziv, id_tar FROM tar_cjen WHERE proizvod='DD' INTO CURSOR tmpcjenici])
	*
	DO CASE &&ostavljeno kad se bude öirilo za druga druötva
		CASE xfqmat_kva
		IF upd_env('', 'DZO_ISKAZNICE')
			PRIVATE xdana_unazad
			xdzo_exp=STRTRAN(PGCENV('DZO_EXP'),'\')
		ENDIF
		*
		DO FORM dzokar_exdo NAME &datafstring
		PROGRESS_CREATE('*** EXPORT DZO KARTICA ***')
		*
		qInsuranceObject.ExportDZOKARTwDOD(ALLTRIM(xcjenik))
	ENDCASE
	PROGRESS_DESTROY()
	USE IN tmpcjenici
	RETURN
ENDPROC && dzokar_exdo


***************************************************************
* Naziv:	Export DZO polica u excel radi izrade zdravstvenih kartica za AT police
* Poziv:	DZOKAR_EXPA()
* Opis:		()
* Autor:	Zrinko
* Datum:	05.07.2017
***************************************************************
PROCEDURE dzokar_expa
dzokar_expu('AT')
RETURN
ENDPROC && dzokar_expa

***************************************************************
* Naziv:	Export DZO polica u excel radi izrade zdravstvenih kartica za AT police PLATINUM
* Poziv:	DZOKAR_EXPLA()
* Opis:		()
* Autor:	Zrinko
* Datum:	03.08.2017
***************************************************************
PROCEDURE dzokar_expla
dzokar_expl('AT')
RETURN
ENDPROC && dzokar_expla


***************************************************************
* Naziv:	Export DZO polica u excel radi izrade zdravstvenih kartica
* Poziv:	DZOKAR_EXPU()
* Opis:		()
* Autor:	Zrinko
* Datum:	05.07.2017
***************************************************************
PROCEDURE dzokar_expu
PARAMETERS pvrst_tar
PRIVATE xvrst_tar, xid_export
xvrst_tar = 0
xid_export = get_ser()
IF PCOUNT()>0
	xvrst_tar = 2
ELSE
	DO WHILE .t.
		xvrst_tar = PGC_RETVAL('Vrsta tarifiranja? 0-ruËna ili 2-automatska:', xvrst_tar)
		IF (xvrst_tar<>0 AND xvrst_tar<>2)
			abc_message('Neispravan unos! Morate unijeti 0 ili 2')
			xvrst_tar = 0
		ELSE
			EXIT
		ENDIF
	ENDDO
ENDIF
PROGRESS_CREATE('*** EXPORT DZO KARTICA ***')
IF qInsuranceObject.ExportDZOKARTu(xvrst_tar, xid_export)
	ml_sql([INSERT INTO dzo_kartexp (id, korisnik, datum) ] +;
		[ VALUES (?xid_export, ?muser, ?DATE() )])
	abc_message('Kreiran je eksport broj ' + ALLTRIM(STR(xid_export,12,0))+'')
ELSE
	abc_message('Export nije uspio ili nema podataka! Pogledajte log datoteku!')
ENDIF
PROGRESS_DESTROY()
RETURN
ENDPROC && dzokar_expu


***************************************************************
* Naziv:	Export DZO polica u excel radi izrade zdravstvenih kartica PLATINUM
* Poziv:	DZOKAR_EXPL()
* Opis:		()
* Autor:	Zrinko
* Datum:	03.08.2017
***************************************************************
PROCEDURE dzokar_expl
PARAMETERS pvrst_tar
PRIVATE xvrst_tar, xid_export
xvrst_tar = 0
xid_export = get_ser()
IF PCOUNT()>0
	xvrst_tar = 2
ELSE
	DO WHILE .t.
		xvrst_tar = PGC_RETVAL('Vrsta tarifiranja? 0-ruËna ili 2-automatska:', xvrst_tar)
		IF (xvrst_tar<>0 AND xvrst_tar<>2)
			abc_message('Neispravan unos! Morate unijeti 0 ili 2')
			xvrst_tar = 0
		ELSE
			EXIT
		ENDIF
	ENDDO
ENDIF
PROGRESS_CREATE('*** EXPORT DZO KARTICA PLATINUM***')
IF qInsuranceObject.ExportDZOKARTplat(xvrst_tar, xid_export)
	ml_sql([INSERT INTO dzo_kartexp (id, korisnik, datum) ] +;
		[ VALUES (?xid_export, ?muser, ?DATE() )])
	abc_message('Kreiran je eksport broj ' + ALLTRIM(STR(xid_export,12,0))+'')
ELSE
	abc_message('Export nije uspio ili nema podataka! Pogledajte log datoteku!')
ENDIF
PROGRESS_DESTROY()
RETURN
ENDPROC && dzokar_expl

***************************************************************
* Naziv:	Punjenje tablice DZO_KART podacima o policama s ugovorenim rizicima DZO
* Poziv:	fill_dzokart()
* Opis:		()
* Autor:	Zrinko PuljiÊ
* Datum:	02.02.2015
***************************************************************
PROCEDURE fill_dzokart
	PRIVATE pfile, xvrs_dat, xstatus, xod, xdo, xdana_unazad, xxml
	*
	xdana_unazad=VAL(STRTRAN(PGCENV('DANA_UNAZAD'),'\'))
	qdana_unazad=IIF(xdana_unazad>0, xdana_unazad, 5)
	*
	xvrs_dat='T' && vrsta datuma / tarifiranja ili izdavanja
	xstatus='T'  && tarifirane, netarifirane ili sve
	xod=DATE()-qdana_unazad
	xdo=DATE()
	ml_sql([CREATE CURSOR ccparam( pvrs_dat C(1), pod D, pdo D, pstatus C(1))])
	*
	pfile = unx_tmp+'punjenje_dzo_kart_log.txt'
	*
	SpremiLog(pfile,'*******************************************************')
	SpremiLog(pfile, '*')
	SpremiLog(pfile, 'POKRENUTO PUNJENJE PODATAKA U TABLICU DZO_KART '+ DTOC(DATE())+' U '+TIME())
	PROGRESS_CREATE('*** Punjenje tablice DZO_KART ***')
	progress.SAY('Prikupljanje kandidata za punjenje')
	*priprema parametara
	ml_sql([INSERT INTO ccparam VALUES (?xvrs_dat, ?xod, ?xdo, ?xstatus)])
	CursorToXML("ccparam","xccparam")
	USE IN ccparam
	*
	progress.SAY('Punjenje tablice DZO_KART')
	progress.SAY('*** *** *** *** ***')
	progress.SAY('Molimo priËekajte...')
	*
	xxml=qInsuranceObject.PunjenjeTabliceDZOKart(xccparam)
	IF xxml>0
		SpremiLog(pfile,'Punjenje podataka zavröeno. Importirano je ' +ALLTRIM(STR(xxml)) + ' polica. ')
	ELSE
		SpremiLog(pfile,'Punjenje podataka zavröeno. Nije importirana niti jedna polica.')
	ENDIF
	progress.SAY('Zavröeno punjenje tablice DZO_KART. Pogledajte log datoteku za detaljnije informacije.')
	PROGRESS_DESTROY()
	SpremiLog(pfile, 'ZAVRäETAK U '+ DTOC(DATE())+' '+TIME()+'')
	SpremiLog(pfile, '*')
	*
	RETURN
ENDPROC && fill_dzokart

***************************************************************
* Naziv:	Unos nove stavke u DZO_KART radi promjena statusa dzo kartice
* Poziv:	STAT_DZKAR('P') ili STAT_DZKAR('S')
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	20.02.2015
***************************************************************
PROCEDURE stat_dzkar
	PARAMETERS psto, pbroj
	PRIVATE ssel, gbroj, xstatus, xserial, xproizvod, xoib, xnaziv,;
		xadresa, xmjesto, xmje_naz, xbr_kart, xmbhzzo, xopis1, xopis2,;
		xopis3, xser_novi, xdatum
	ssel=SELECT()
	*
	IF PCOUNT() == 2
		gbroj=ALLTRIM(pbroj)
	ELSE
		gbroj=ALLTRIM(broj)
	ENDIF
	IF psto<>'P' AND psto<>'S'
		abc_message('Neispravan parametar u pozivu funkcije')
		SELECT (ssel)
		RETURN
	ENDIF
	xdatum=DATE()
	ml_sql([SELECT * FROM dzo_kart WHERE serial in (select max(serial) ] +;
		[ AS serial from dzo_kart WHERE polica=']+gbroj+[') INTO CURSOR podloga])
	IF _tally=1
		xstatus=ALLTRIM(status)
		xserial=serial
		xproizvod=ALLTRIM(proizvod)
		xoib=ALLTRIM(oib)
		xnaziv=ALLTRIM(naziv)
		xadresa=ALLTRIM(adresa)
		xmjesto=ALLTRIM(mjesto)
		xmje_naz=ALLTRIM(mje_naz)
		xbr_kart=ALLTRIM(br_kart)
		xmbhzzo=ALLTRIM(mbhzzo)
		xopis1=ALLTRIM(opis1)
		xopis2=ALLTRIM(opis2)
		xopis3=ALLTRIM(opis3)
		IF psto='P'
			DO CASE
				CASE xstatus='S'
					abc_message('Kartica je stornirana!')
				CASE xstatus='P'
					abc_message('Kartica je veÊ oznaËena za ponovni tisak!')
				CASE xstatus='T'
					IF abc_upit('Y','éelite li ponovno tiskati DZO karticu za policu ' + ALLTRIM(gbroj) + '?')
						xser_novi=get_ser()
						ml_sql([INSERT INTO dzo_kart (serial, proizvod, polica, oib, naziv, adresa, ] +;
							[ mjesto, mje_naz, dat_file, dat_slanja, poslano, status, dat_stor, br_kart, rbr, ] +;
							[ mbhzzo, opis1, opis2, opis3) ] +;
							[ VALUES (?xser_novi, ?xproizvod, ?gbroj, ?xoib, ?xnaziv, ?xadresa, ?xmjesto, ?xmje_naz, ] +;
							[ ?qempty_date, ?qempty_date, 0, 'P', ?qempty_date, ?xbr_kart, 0, ?xmbhzzo, ?xopis1, ?xopis2, ?xopis3)])
					ENDIF
			ENDCASE
		ELSE
			IF xstatus='S'
				abc_message('Kartica je veÊ stornirana!')
			ELSE
				IF abc_upit('Y','éelite li stornirati DZO karticu za policu ' + ALLTRIM(gbroj) + '?')
					xser_novi=get_ser()
					ml_sql([INSERT INTO dzo_kart (serial, proizvod, polica, oib, naziv, adresa, ] +;
						[ mjesto, mje_naz, dat_file, dat_slanja, poslano, status, dat_stor, br_kart, rbr, ] +;
						[ mbhzzo, opis1, opis2, opis3) ] +;
						[ VALUES (?xser_novi, ?xproizvod, ?gbroj, ?xoib, ?xnaziv, ?xadresa, ?xmjesto, ?xmje_naz, ] +;
						[ ?qempty_date, ?qempty_date, 0, 'S', ?xdatum, ?xbr_kart, 0, ?xmbhzzo, ?xopis1, ?xopis2, ?xopis3)])
				ENDIF
			ENDIF
		ENDIF
		USE IN podloga
	ELSE
		abc_message('Ne postoji kartica za policu broj '+gbroj+'!')
	ENDIF
	*
	SELECT (ssel)
	RETURN
ENDPROC && stat_dzkar

***************************************************************
* Naziv:	Storniranje kartice DZO (nad policom)
* Poziv:	STOR_DZKAR()
* Opis:		()
* Autor:	Zrinko PuljiÊ
* Datum:	05.10.2016
* Dorade:	29.05.2020. poziv dll funkcije za izradu nove DZO kartice (Maja)
*			22.07.2020. poziv dll funkcije za storniranje DZO kartice (Maja)
* 			30.11.2020. stornira se samo zapis nad kojim je pozvana akcija (Maja)
*			21.12.2020. izbaËena proöla promjena (Maja)
***************************************************************
PROCEDURE stor_dzkar
PRIVATE ssel, gbroj, gproizvod, xbr_kart, xdatum, xstatus_n, xstatus_akt
ssel=SELECT()
*
upd_env('','DZO_ISKAZNICE')
xstatus_n=STRTRAN(PGCENV('DZO_KART_SI'),'\')
xstatus_akt=STRTRAN(PGCENV('DZO_KART_AKT'),'\')
*
gbroj=ALLTRIM(broj)
gproizvod = proizvod
xdatum=DATE()
*uzmi podatke od zadnje kartice za policu
ml_sql([SELECT * FROM dzo_kart WHERE serial in (select max(serial) ] +;
		[ AS serial from dzo_kart WHERE polica=']+gbroj+[' AND status NOT LIKE 'S%') INTO CURSOR cczadnja_kart])
IF _TALLY=1
	xbr_kart=ALLTRIM(br_kart)
	IF abc_upit('Y','éelite li stornirati posljednju izdanu karticu ('+ALLTRIM(xbr_kart)+') i napraviti novu DZO karticu za policu ' + ALLTRIM(gbroj) + '?')
		qInsuranceObject.storno_kart(xbr_kart, xstatus_n, xdatum)
		gen_dzo_kart((gbroj), (gproizvod))
		abc_message('Kreirana je nova kartica.')
		USE IN cczadnja_kart
	ENDIF
ELSE
	abc_message('Ne postoji aktivna kartica za policu broj '+gbroj+'!')
ENDIF
*
SELECT (ssel)
RETURN
ENDPROC && stor_dzkar


***************************************************************
* Naziv:	Storniranje kartice tisak DZO (storno tisak) (nad karticom)
* Poziv:	stor_dzktis() - kreira se nova kartica s istim podacima kao i stara, osim refresh osiguranika
*			stor_dzktis(.T.) - kreira se potpuno nova kartica s istim brojem kao i stara koja se stornira
* Opis:		()
* Autor:	Zrinko PuljiÊ
* Datum:	09.11.2016
* Dorade:	29.05.2020. za Wiener se poziva dll funkcija za izradu nove DZO kartice (Maja)
*			22.07.2020. poziv dll funkcije za storniranje DZO kartice (Maja)
* 			30.11.2020. stornira se samo zapis nad kojim je pozvana akcija (Maja)
*			21.12.2020. za generiranje nove kartice zove se druga metoda (Maja)
***************************************************************
PROCEDURE stor_dzktis
PARAMETERS psve
PRIVATE ssel, gbroj, xbr_kart, xser_novi, xdatum, xstatus_n, xstatus_akt, ;
	xdost_adr, xdost_naziv, xdost_mje, xdost_ptt, xosi_id, xvar, xsve
ssel = SELECT()
*
xsve = IIF(PCOUNT()<1, .F., .T.)
xvar = pgc_var('G')
PRIVATE &xvar
STORE '' TO &xvar
izj_nul('IZJ', 'G', '*')
*
upd_env('','DZO_ISKAZNICE')
xstatus_n = STRTRAN(PGCENV('DZO_KART_SP'),'\')
xstatus_akt = STRTRAN(PGCENV('DZO_KART_AKT'),'\')
*
xdatum = DATE()
xosi_id = ret_xt1('osi_id', 'poli02', [broj='] + ALLTRIM(gpolica) + ['], '')
*
gbr_kart = br_kart
*
IF abc_upit('Y', pgc_dlang('éelite li oznaËiti karticu za pononvno slanje?')+CHR(13)+CHR(13)+' ('+ALLTRIM(gbr_kart)+')')
	*
	IF (xfqmat_tri AND xsve AND status='A') OR xfqmat_kva
		* Marko (24.12.2019), Maja (08.01.2020.) - storno stare i generiranje nove DZO iskaznice (OSTAJE ISTI BROJ KARTICE, AéURIRA SE TRAJANJE KARTICE)
		qInsuranceObject.storno_kart1(gbr_kart, gpolica, xstatus_n, xdatum)
		qInsuranceObject.PonovnoKreirajNovuDZOKarticu((gpolica), (gproizvod), (gbr_kart), .T.)
	ELSE
		ml_sql([UPDATE dzo_kart SET status=?xstatus_n, korisnik=?muser, dat_obr=?xdatum, dat_stor=?xdatum ]+;
			[ WHERE br_kart=?gbr_kart AND polica=?gpolica AND status NOT LIKE 'S%'])
		xser_novi = get_ser()
		ml_sql([SELECT naziv, adresa, mjesto, dost_naziv, dost_adr, dost_ptt, dost_mje ]+;
			[ FROM partneri ]+;
			[ WHERE jmbg=?xosi_id ]+;
			[ INTO CURSOR t_part1])
		IF _TALLY>0
			IF !EMPTY(dost_adr)
				xdost_adr = dost_adr
				xdost_naziv = dost_naziv
				xdost_ptt = dost_ptt
				xdost_mje = dost_mje
			ELSE
				xdost_adr = adresa
				xdost_naziv = naziv
				xdost_ptt = mjesto
				xdost_mje = ret_xt1('naziv', 'mjesta', [ptt_br='] + ALLTRIM(xdost_ptt) + ['], '')
			ENDIF
		ELSE
			xdost_adr = gdost_adr
			xdost_naziv = gdost_naziv
			xdost_ptt = gdost_ptt
			xdost_mje = gdost_mje
		ENDIF
		USE IN t_part1
		*
		ml_sql([INSERT INTO dzo_kart (status, serial, proizvod, polica, oib, naziv, adresa, ] +;
			[ mjesto, mje_naz, dat_file, dat_slanja, poslano, dat_stor, br_kart, rbr, ] +;
			[ mbhzzo, opis1, opis2, opis3, vrsta, korisnik, dat_obr, dat_poc, dat_ist, datum, ]+;
			[ dost_adr, dost_naziv, dost_mje, dost_ptt, spol, at, id_exp, dat_file_imp) ] +;
			[ VALUES (?xstatus_akt, ?xser_novi, ?gproizvod, ?gpolica, ?goib, ?gnaziv, ?gadresa, ] +;
			[ ?gmjesto, ?gmje_naz, ?qempty_date, ?qempty_date, 0, ?qempty_date, ?gbr_kart, 0, ] +;
			[ ?gmbhzzo, ?gopis1, ?gopis2, ?gopis3, ?gvrsta, ?muser, ?gdatum, ?gdat_poc, ?gdat_ist, ] +;
			[ ?gdatum, ?xdost_adr, ?xdost_naziv, ?xdost_mje, ?xdost_ptt, ?gspol, ?gat, ?gid_exp, ] +;
			[ ?gdat_file_imp)])
	ENDIF
	abc_message(pgc_dlang('UPOZORENJE')+CHR(13)+CHR(13)+;
		pgc_dlang('Kartica je oznaËena za ponovno slanje')+'!')
ENDIF
*
SELECT (ssel)
RETURN
ENDPROC && stor_dzktis


***************************************************************
* Naziv:	Storniranje retka iskaznice DZO (storno tisak) (nad karticom)
* Poziv:	STOR_DZKRED()
* Opis:		()
* Autor:	Zrinko PuljiÊ
* Datum:	10.02.2018.
***************************************************************
PROCEDURE stor_dzkred
PRIVATE ssel, xbr_kart, xstatus_n, xdat_stor
ssel=SELECT()
*
xvar = pgc_var('G')
PRIVATE &xvar
STORE '' TO &xvar
izj_nul('IZJ', 'G', '*')
*
upd_env('','DZO_ISKAZNICE')
xstatus_n=STRTRAN(PGCENV('DZO_KART_SP'),'\')
*
gserial = serial
xdat_stor = DATE()
*
IF abc_upit('Y','éelite li poniötiti redak za karticu broj ('+ALLTRIM(gbr_kart)+')?')
	xdat_stor = PGC_RETVAL('Datum storna:',xdat_stor)
	ml_sql([UPDATE dzo_kart SET status=?xstatus_n, korisnik=?muser, dat_obr=?DATE(), dat_stor=?xdat_stor WHERE ] +;
		[ serial=?gserial AND status NOT LIKE 'S%'])
		*
	abc_message('Kartica je poniötena!')
ENDIF
*
SELECT (ssel)
RETURN
ENDPROC && stor_dzkred

***************************************************************************
* Funkcija: UGOTN_I
* Autor: Zrinko PuljiÊ
* Datum: 25.11.2013.
* Opis : Import ugovora o trajnim nalozima
***************************************************************************
PROCEDURE UGOTN_I
PARAMETERS ptip, pbanka  &&tip datoteke, sifra banke (3-ZABA, 4-RBA, 5-OTP, 7-podravska, 8-hypo/addico, 9-poötanska)
UGOTN_IMPORT(ptip, pbanka)
RETURN

PROCEDURE UGOTN_ZABA
UGOTN_I(1,3)
RETURN

PROCEDURE UGOTN_RBA
UGOTN_I(1,4)
RETURN

PROCEDURE UGOTN_OTP
UGOTN_I(1,5)
RETURN

PROCEDURE UGOTN_ERS
UGOTN_I(1,0)
RETURN

PROCEDURE UGOTN_PBZ
UGOTN_I(1,1)
RETURN

PROCEDURE UGOTN_SB
UGOTN_I(1,2)
RETURN

PROCEDURE UGOTN_POB
UGOTN_I(1,7)
RETURN

PROCEDURE UGOTN_HYPO
UGOTN_I(1,8)
RETURN

PROCEDURE UGOTN_PB
UGOTN_I(1,9)
RETURN


PROCEDURE UGOTN_AMEX
UGOTN_I(1,6)
RETURN

PROCEDURE UGOTN_DIN
UGOTN_I(1,10)
RETURN

PROCEDURE TN_DATM
TN_DAT('M')
RETURN

PROCEDURE TN_IMPORTM
TN_IMPORT('M')
RETURN

PROCEDURE TN_UPLE
DO TN_UPLE IN TN_IMPORT
RETURN

***************************************************************************
* Funkcija: VratiSurZaAdmina
* Autor: Nikola MilkoviÊ
* Datum: 21.03.2015.
* Opis : VraÊa sifre suradnika za administratora
* Izmjena: Marko (24.03.2016) - qsuradnik umjesto QCubisObject.id_sura
***************************************************************************
PROCEDURE VratiSurZaAdmina
PRIVATE ssel, xadmin
ssel = SELECT()
xadmin = ''
ML_SQL([SELECT sifra FROM blag_se WHERE login = ?muser and aakt = 1 INTO CURSOR ttemsif])
xsifra = sifra
USE IN ttemsif
IF !EMPTY(xsifra)
	ML_SQL([ select id_sura from suradnci where ablag_se=?xsifra INTO CURSOR ttemsif ] )
	SELECT ttemsif
	GO TOP
	SCAN ALL
		xadmin = xadmin + ALLTRIM(id_sura) + ','
	ENDSCAN
	xadmin = LEFT(xadmin, LEN(xadmin)-1)
	USE IN ttemsif
ELSE
	xadmin = qsuradnik		&& QCubisObject.id_sura
ENDIF
SELECT(ssel)
RETURN xadmin

***************************************************************************
* Funkcija: VratiSifruAdmina
* Autor: Nikola MilkoviÊ
* Datum: 21.03.2015.
* Opis : VraÊa sifre administratora
* Izmjena: Marko (24.03.2016) - qsuradnik umjesto QCubisObject.qsuradnik
***************************************************************************
PROCEDURE VratiSifruAdmina
PRIVATE ssel, xadmin, xsura
ssel = SELECT()
xadmin = ''
xsura = qsuradnik		&& QCubisObject.qsuradnik
ML_SQL([SELECT b.sifra FROM blag_se b, suradnci s WHERE b.sifra=s.ablag_se AND b.aakt=1 AND s.id_sura=?xsura INTO CURSOR ttemsif])
IF _tally > 0
	xadmin = sifra
ENDIF
USE IN ttemsif
SELECT(ssel)
RETURN xadmin

***************************************************************************
* Funkcija: VratiSurZaVoditelja
* Autor: Nikola MilkoviÊ
* Datum: 21.03.2015.
* Opis : VraÊa sifre suradnika za voditelja
* Izmjena: Marko (24.03.2016) - qsuradnik umjesto QCubisObject.id_sura
***************************************************************************
PROCEDURE VratiSurZaVoditelja
PRIVATE ssel, xadmin, xsura
PRIVATE ssel, xadmin
ssel = SELECT()
xadmin = ''
ML_SQL([SELECT ablag_se FROM suradnci WHERE login = ?muser INTO CURSOR ttemsif])
xsifra = ablag_se
USE IN ttemsif
IF !EMPTY(xsifra)
	ML_SQL([ select id_sura from suradnci where ablag_se=?xsifra INTO CURSOR ttemsif ] )
	SELECT ttemsif
	GO TOP
	SCAN ALL
		xadmin = xadmin + ALLTRIM(id_sura) + ','
	ENDSCAN
	xadmin = LEFT(xadmin, LEN(xadmin)-1)
	USE IN ttemsif
ELSE
	xadmin = qsuradnik		&& QCubisObject.id_sura
ENDIF
SELECT(ssel)
RETURN xadmin

***************************************************************************
* Funkcija: VratiAdminaBlagajne
* Autor: Nikola MilkoviÊ
* Datum: 21.03.2015.
* Opis : VraÊa sifru administratora blagajne
***************************************************************************
PROCEDURE VratiAdminaBlagajne
PRIVATE ssel, xadmin, xsura
ssel = SELECT()
xadmin = ''
ML_SQL([SELECT sifra FROM blag_se WHERE login=?muser AND akt = 1 INTO CURSOR ttemsif])
IF _tally > 0
	xadmin = sifra
ENDIF
USE IN ttemsif
SELECT(ssel)
RETURN xadmin

***************************************************************************
* Funkcija: VratiSuradnikePoPS
* Autor: Nikola MilkoviÊ
* Datum: 21.03.2015.
* Opis : VraÊa sifru suradnika po prodajnoj strukturi
***************************************************************************
PROCEDURE VratiSuradnikePoPS
PRIVATE ssel, xsura
ssel = SELECT()
xsura =  QInsuranceObject.VratiSuradnikePoPS()
SELECT(ssel)
RETURN xsura

***************************************************************************
* Funkcija: VratiSuradnikePoNPS
* Autor: Marko Bekafigo
* Datum: 24.03.2016.
* Opis : VraÊa sifru suradnika po nadreenoj prodajnoj strukturi
***************************************************************************
PROCEDURE VratiSuradnikePoNPS
PRIVATE ssel, xsura
ssel = SELECT()
xsura =  QInsuranceObject.VratiSuradnikePoNPS()
SELECT(ssel)
RETURN xsura

***************************************************************************
* Funkcija: VratiSuradnikePoJmbg
* Autor: Nikola MilkoviÊ
* Datum: 21.03.2015.
* Opis : VraÊa sifru suradnika po jmbg-u
***************************************************************************
PROCEDURE VratiSuradnikePoJmbg
PRIVATE ssel, xsura
ssel = SELECT()
xsura =  QInsuranceObject.VratiSuradnikePoJmbg(muser)
SELECT(ssel)
RETURN xsura

*******************************************************************************
* Incubis d.o.o.   	              ctrl_ovrsenik         (c) All rights reserved
* Author......: Marko Bekafigo                                 Date: 25.04.2015
* Description.: Obnova polica ovröenika
* Dorada......:
*******************************************************************************
PROCEDURE ctrl_ovrsenik
PARAMETERS pugo_id, prata, ppolic_pon
PRIVATE ssel, xok, xporuka
ssel = SELECT()
*
xok = .F.
xporuka = ''
*
* Marko (09.05.2015) - Dodao filter samo za neke vrste osiguranja (AO, AK) - #OVRSENIK_VRSTE=03;10
IF xctrl_ovrsenik AND (EMPTY(xovrsenik_vrste) OR (VARTYPE(sosn_vrst)='C' AND sosn_vrst$xovrsenik_vrste)) AND VARTYPE(xtip_pp)='C' AND xtip_pp='POLICE'
	xok = QInsuranceObject.CheckObnovaOvrsenikPoruka(pugo_id, prata, ppolic_pon, ('+OBNOVR+'$qovlasti), xporuka, .T., (ssifra))
	IF !EMPTY(xporuka) 	&& xok
		abc_message(xporuka)
	ENDIF
ENDIF
*
SELECT (ssel)
RETURN xok
ENDPROC && ctrl_ovrsenik

*******************************************************************************
* Incubis d.o.o.   	                 zatv_roc           (c) All rights reserved
* Author......: Marko Bekafigo                                 Date: 25.04.2015
* Description.: Zatvaranje ötete (akcija nad roËiönikom)
* Dorada......:
*******************************************************************************
PROCEDURE zatv_roc
	PARAMETERS psto
	PRIVATE ssel, xknjiga, xsteta, xupdroc
	ssel = SELECT()
	*
	* Marko(21.04.2015) - zatvaranje ötete u roËiöniku
	xupdroc = .F.
	IF PCOUNT()=1 AND psto='S'
		xknjiga = knjiga
		xsteta = broj
		IF xrocisnik
			ml_sql([SELECT * FROM rocisnik ]+;
				[ WHERE knjiga=?xknjiga AND steta=?xsteta AND zatvorena=0 ]+;
				[ INTO CURSOR tmproc])
			IF _TALLY>0
				xupdroc = .T.
			ENDIF
			USE IN tmproc
		ENDIF
	ELSE
		xknjiga = knjiga
		xsteta = steta
		IF xrocisnik AND zatvorena=0
			xupdroc = .T.
		ENDIF
	ENDIF
	*
	IF xupdroc
		ml_sql([UPDATE rocisnik ]+;
			[ SET zatvorena=1, dat_zatv=?DATE(), korisnik=?muser ]+;
			[ WHERE knjiga=?xknjiga AND steta=?xsteta])
		abc_message('äteta '+ALLTRIM(xknjiga)+'/'+ALLTRIM(xsteta)+' je zatvorena u roËiöniku')
	ENDIF
	*
	SELECT (ssel)
	RETURN
ENDPROC && zatv_roc


*******************************************************************************
* Incubis d.o.o.   	                 zaprneisp           (c) All rights reserved
* Author......: Nikola MilkoviÊ                                 Date: 09.06.2015
* Description.: Zaprimanje tarifno neispravne police
* Dorada......:
*******************************************************************************
PROCEDURE zaprneisp
	PRIVATE ssel, xbroj, xstatusi, xporuka, xproizvod, xopis, xold_stat
	ssel = SELECT()
	*
	xbroj = broj
	xproizvod = proizvod
	xold_stat = id_status
	xstatusi =qzaprneis
	xporuka = 	'Jeste li sigurni da ûelite oznaËiti policu ' + CHR(13) +  ;
				'kao zaprimljenu tarifiranu neispravnu?'
	IF !EMPTY (qtar_neispr) AND UPPER(ALLTRIM(id_status))!= UPPER(ALLTRIM(qtar_neispr)) AND ALLTRIM(UPPER(id_status)) $ xstatusi AND abc_upit('Y',xporuka)
		ml_sql([ SELECT distinct status FROM STAT_POL WHERE BROJ = ?xbroj and proizvod =?xproizvod  INTO CURSOR tstat])
		LOCATE FOR UPPER(ALLTRIM(status)) == UPPER(ALLTRIM(qkont_djeraz))
		IF !FOUND() && dodaj status djelomiËno razduûena
			xopis = ret_ext(qkont_djeraz, 'STATUS_P', , 'OPIS', 'SIFRA')
			INSERT_STATUS('stat_pol', xproizvod, xbroj, qkont_djeraz, DATE(), xopis)
		ENDIF
		GO TOP
		LOCATE FOR UPPER(ALLTRIM(status)) == UPPER(ALLTRIM(qkont_zapdj))
		IF !FOUND() && dodaj status djelomiËno zaprimljena
			xopis = ret_ext(qkont_zapdj, 'STATUS_P', , 'OPIS', 'SIFRA')
			INSERT_STATUS('stat_pol', xproizvod, xbroj, qkont_zapdj, DATE(), xopis)
		ENDIF
		GO TOP
		*postavi status TN
		xopis = ret_ext(qtar_neispr, 'STATUS_P', , 'OPIS', 'SIFRA')
		qInsuranceObject.PromjeniStatusPolice('poli10','stat_pol', xproizvod, xbroj, xold_stat, qtar_neispr, DATE(), xopis,DATE(1901,01,01),DATE(1901,01,01))
	ELSE
		abc_message('Nisu zadovoljeni uvjeti za postavljanje statusa.')
	ENDIF
	*
	SELECT (ssel)
	RETURN .T.
ENDPROC && zatv_roc

*******************************************************************************
* Incubis d.o.o.   	                 save_polpdf          (c) All rights reserved
* Author......: Nikola MilkoviÊ                                 Date: 17.06.2015
* Description.: Spremanje PDF-a police u bazu
* Dorada......:
*******************************************************************************
PROCEDURE save_polpdf
	PARAMETERS ppath
	PRIVATE ssel, xmemorystream
	ssel = SELECT()
	*
	IF FILE(ppath) && postoji datoteka
		xmemorystream = QInsuranceObject.pripstream_polAO(ppath)
		IF VARTYPE(xmemorystream)=='O'
			QCubisObject.ArhivaTiska(xmemorystream)
			TRY && probaj pobrisati file
				ERASE &ppath
			CATCH
				*do nothing
			ENDTRY
		ENDIF
	ELSE

	ENDIF && postoji datoteka
	*
	SELECT (ssel)
	RETURN .t.
ENDPROC && save_polpdf

*******************************************************************************
* Incubis d.o.o.   	        ArhivaTiskaPolice           (c) All rights reserved
* Author......: Andreas CrnkoviÊ                               Date: 09.07.2018
* Description.: Spremanje PDF-a police u bazu
* Dorada......:
*******************************************************************************
PROCEDURE ArhivaTiskaPolice
	PARAMETERS pnazivreporta, pxml, pbroj, pproizvod
	PRIVATE ssel, xstream
	ssel = SELECT()
	*
	IF ssifra $ qarhiva_proizvodi
		qCubisObject.tisarhbroj= pbroj
		qCubisObject.tisarhproizvod= pproizvod
		*
		xstream = qCubisObject.pgc_outstream(pnazivreporta, pxml)
		qCubisObject.ArhivaTiska(xstream)
	ENDIF
	*
	SELECT (ssel)
	RETURN .t.
ENDPROC && ArhivaTiskaPolice

*******************************************************************************
* Incubis d.o.o.   	        ArhivaTiskaFakture          (c) All rights reserved
* Author......: Andreas CrnkoviÊ                               Date: 02.02.2017
* Description.: Spremanje PDF-a fakture u bazu
* Dorada......:
*******************************************************************************
PROCEDURE ArhivaTiskaFakture
	PARAMETERS pbrojfakture, pnazivreporta, pxml
	PRIVATE ssel, xstream
	ssel = SELECT()
	*
	pnazivreporta = UPPER(ALLTRIM(pnazivreporta))
	xstream = qCubisObject.pgc_outstream(pnazivreporta, pxml)
	*
	IF xfqmat_kva
		IF INLIST(pnazivreporta, 'TISFAKTKVF3', 'TISFAKTBKV3', 'TISFAKTSPO3', 'TISFAKTSP3', 'TISFAKWDD')
			xstream = qReportsObject.WriteHUB(xstream, 8, 35, 580, 240, .T.)
		ELSE
			xstream = qReportsObject.WriteCompanyLogo(xstream)
		ENDIF
	ELSE
		IF sektor='1'
			IF xfqmat_hok
				xstream = qReportsObject.WriteHUB(xstream, 8, 30, 580, 240, .T.)
			ELSE
				xstream = qReportsObject.WriteHUB(xstream, 8, 35, 580, 240, .T.)
			ENDIF
		ELSE
			xstream = qReportsObject.WriteCompanyLogo(xstream)
		ENDIF
	ENDIF
	qCubisObject.ArhivaTiskaFakture(pbrojfakture, xstream)
	*
	SELECT (ssel)
	RETURN .t.
ENDPROC && ArhivaTiskaFakture

*******************************************************************************
* Incubis d.o.o.   	      PosaljiFakturuMessengeru      (c) All rights reserved
* Author......: Andreas CrnkoviÊ                               Date: 02.02.2017
* Description.: Spremanje fakture u msg_log za slanje mailom
* Dorada......:
*******************************************************************************
PROCEDURE PosaljiFakturuMessengeru
	PARAMETERS pBrojFakture, pVrstaZapisa
	PRIVATE ssel, xstream, xposiljatelj, xprimatelj, xsubject, xporuka, xvrsta_zapisa, xkanal, xserial
	ssel = SELECT()
	xvrsta_zapisa=''
	xserial=''
	xposiljatelj=ALLTRIM(STRTRAN(PGCENV('fakture_posiljatelj'),'\'))
	xsubject=ALLTRIM(STRTRAN(PGCENV('fakture_subject'),'\'))
	DIMENSION xarr_filename(1)
	*
	PUBLIC qCubisObject2 AS "Cubis.cubis"
	PUBLIC qCubisObject3 AS "Cubis.cubis"
	*
	DO CASE
		CASE pVrstaZapisa=='R'
			xvrsta_zapisa='00009'
			IF PGCENV('ERACUN','FINA') OR PGCENV('ERACUN','FINAB2G')
				xprimatelj="eRaËun"
			ELSE
				xprimatelj="Moj-eRaËun"
			ENDIF
			xporuka=pBrojFakture
			xarr_filename(1)=""
			xkanal='00002'
		CASE pVrstaZapisa=='O'
			xvrsta_zapisa='00010'
			xprimatelj="eOdobrenje"
			xporuka=pBrojFakture
			xarr_filename(1)=""
			xkanal='00002'
		CASE pVrstaZapisa=='E'
			xvrsta_zapisa='00006'
			xprimatelj=ALLTRIM(RET_XT1('erac_mail', 'fakture f, partneri p', [p.jmbg=f.id_part and f.broj=']+pBrojFakture+['], SPACE(8)))
			IF EMPTY(xprimatelj)
				xprimatelj=ALLTRIM(RET_XT1('e_mail', 'fakture f, partneri p', [p.jmbg=f.id_part and f.broj=']+pBrojFakture+['], SPACE(8)))
			ENDIF
			xporuka=FILETOSTR(ALLTRIM(STRTRAN(STRTRAN(PGCENV('fakture_poruka'),'\'),';',',')))
			xarr_filename(1)="_#"+ALLTRIM(pBrojFakture)
			xkanal='00003'
	ENDCASE

	DO spremi_poruku WITH xposiljatelj, xprimatelj, xsubject, xporuka, xvrsta_zapisa, xarr_filename, xkanal IN msg_log
	*
	SELECT (ssel)
	RETURN xserial
ENDPROC && PosaljiFakturuMessengeru

PROCEDURE BrisiIzMessengera
PRIVATE xid
	xid=id
	QInsuranceObject.BrisiIzMessengera(xid)
ENDPROC

PROCEDURE ResendMojERacun
PRIVATE ximp_file, xret, xmessage
	ximp_file = GETFILE('xml','','',0,'XML datoteka za eRaËun')
	IF !EMPTY(ximp_file) AND FILE(ximp_file)
		xstrfile=FILETOSTR(ximp_file)
		xret = qReportsObject.MojeRacun_SendXML_WinCubis(xstrfile)
		xmessage = IIF(EMPTY(xret), "Uspjeöno poslano!", xret)
	ELSE
		xmessage = "Ne postoji datoteka!"
	ENDIF
	abc_message(xmessage)
ENDPROC

PROCEDURE KreirajXmlERacun
PARAMETERS pInvoice_type
PRIVATE xBr_fakt, xmsg_id, xpath, xInvoice_type
	xBr_fakt=poruka
	xmsg_id=id
	xInvoice_type = IIF(PCOUNT()=0, '380', pInvoice_type)
	xxml = qReportsObject.KreirajXmlERacun(xBr_fakt, xInvoice_type, xmsg_id)
	IF EMPTY(xxml)
		abc_message('Doölo je do pogreöke u kreiranju XML-a: ' + qReportsObject.Error)
	ELSE
		xpath = qReportsObject.SpremiXmlERacunNaDisk(xBr_fakt, xxml)
		abc_message('XML je spremljen u datoteku: ' + xpath)
	ENDIF
ENDPROC

*******************************************************************************
* Incubis d.o.o.   	  PosaljiObavijestMessengeru    (c) All rights reserved
* Author......: Andreas CrnkoviÊ                               Date: 02.02.2017
* Description.: Spremanje obavijesti u msg_log za slanje mailom
* Dorada......:
*******************************************************************************
PROCEDURE PosaljiObavijestMessengeru
	PARAMETERS pNazivReporta, pXML, pPartner, pbroj
	PRIVATE ssel, xstream, xposiljatelj, xprimatelj, xsubject, xporuka, xvrsta_zapisa, xkanal
	ssel = SELECT()
	DIMENSION xarr_filename(1)
	*
	PUBLIC qCubisObject2 AS "Cubis.cubis"
	PUBLIC qCubisObject3 AS "Cubis.cubis"
	*
	xvrsta_zapisa='00008'
	xposiljatelj=ALLTRIM(STRTRAN(PGCENV('obavijest_posiljatelj'),'\'))
	xsubject=ALLTRIM(STRTRAN(PGCENV('obavijest_subject'),'\')) + ' ' + ALLTRIM(pbroj)
	xporuka=FILETOSTR(ALLTRIM(STRTRAN(STRTRAN(PGCENV('obavijest_poruka'),'\'),';',',')))
	xprimatelj=RET_XT1('e_mail', 'partneri', [jmbg=']+pPartner+['], SPACE(8))
	xarr_filename(1)=qCubisObject.pgc_outstream(pNazivReporta, pXML)
	xkanal='00003'

	DO spremi_poruku WITH xposiljatelj, xprimatelj, xsubject, xporuka, xvrsta_zapisa, xarr_filename, xkanal IN msg_log
	*
	SELECT (ssel)
	RETURN .t.
ENDPROC && PosaljiObavijestMessengeru

********************************************************************************
FUNCTION tisstorras
PRIVATE ssel, xduzna, xbr_uvj, xdat_vrij, xdat_unos
ssel = SELECT()
*
STORE {} TO xdat_vrij, xdat_unos
xbr_uvj =  IIF(SUBSTR(gbroj, 11, 1) == '-', [ polica LIKE '] + LEFT(gbroj, 11) + [%'], [ polica = '] + gbroj + ['])
*
ml_sql([SELECT SUM(iznos - iznosp) AS duzna FROM st_prem WHERE ]+ xbr_uvj +[ INTO CURSOR tduzna])
xduzna = duzna
USE IN tduzna
*
ml_sql([SELECT TOP 1 dat_vrij, dat_unos FROM stat_pol WHERE broj=?gbroj AND status=?gid_status INTO CURSOR tmpdatumi])
IF _TALLY>0
	xdat_vrij = dat_vrij
	xdat_unos = dat_unos
ENDIF
USE IN tmpdatumi
*
DO tisobav_ras IN raskpol WITH gbroj, xduzna , xdat_vrij, xdat_unos, .F., gist_dat
*
SELECT(ssel)
RETURN
ENDFUNC && tisstorras

*******************************************************************************
* Incubis d.o.o.   	              prom_mvprpol          (c) All rights reserved
* Author......: Marko Bekafigo                                 Date: 02.09.2015
* Description.: Promjena podatak predmetu osiguranja
*				(npr. o plovilu, letjelici, itd.)
* Dorada......: Ozren äirola								   Date: 31.03.2017
*******************************************************************************
PROCEDURE prom_mvprpol
	PRIVATE ssel, x, xvar, xser, xtemp, xok, aid_nam, aid_tip, avrsobj, anamobj, ;
		akatobj1, akatobj2, akatopr1, akatopr2, avrsizgr, azgrada, apotkr, akrov, ;
		akatobjap1, avrs_brod, anam_brod, arazpotr, agran_plov, xbroj, xproizvod
	ssel = SELECT()
	*
	xproizvod = proizvod
	xbroj = broj
	*
	DIMENSION aid_nam(4,2), aid_tip(2,2)
	DIMENSION avrsobj(1), anamobj(1), akatobj1(1), akatobj2(1), akatopr1(1), akatopr2(1), avrsizgr(1), azgrada(1), apotkr(1), akrov(1), akatobjap1(1), arazpotr(1)
	DIMENSION avrs_brod(IIF(xfqmat_uni, 3, IIF(xfqmat_kva, 10, 5)), 1)
	DIMENSION anam_brod(IIF(xfqmat_uni OR xfqmat_kva, 4, 3), 1)
	DIMENSION agran_plov(IIF(xfqmat_kva, 15, 1), 1)
	agran_plov(1,1)=' '
	*
	fill_pred_sifar(spred_pol, ssifra)
	xok = .F.
	*
	DO CASE
		CASE spred_pol='IMOPRPOL'

		CASE spred_pol='MVPRPOL'

			* Podaci o predmetu osiguranja
			ml_sql([SELECT * FROM ]+xtab_pred+[ WHERE proizvod=?xproizvod AND polica=?xbroj INTO CURSOR tmpmvprpol READWRITE])
			IF _TALLY>0
				SELECT tmpmvprpol
				xvar = PGC_VAR()
				PRIVATE &xvar
				STORE '' TO &xvar
				izj_nul('IZJ','G','*')
				*
				IF xfqmat_kva
					DO CASE
						CASE INLIST(sosn_vrst, '05', '11')
							* predmet osiguranja zrakoplov
							DO FORM mvprpol2
						CASE INLIST(sosn_vrst, '07')
							* predmet osiguranja roba u prijevozu
							DO FORM mvprpol3
						CASE INLIST(ssifra, 'PO')
							* predmet osiguranja prijevoznikova odgovornost
							DO FORM mvprpol4
						OTHERWISE
							* predmet osiguranja motorna vozila
							DO FORM mvprpol
					ENDCASE
				ELSE
					* predmet osiguranja motorna vozila
					DO FORM mvprpol
				ENDIF
				*
				IF xok
					ml_sql([UPDATE ]+xtab_pred+[ SET ]+;
						[ imebroda=?gimebroda, vrsta_br=?gvrsta_br, reg_ozn=?greg_ozn, vrsta_mat=?gvrsta_mat, ]+;
						[ tipmodel=?gtipmodel, god_pro=?ggod_pro, marka=?gmarka, ce_ser_br=?gce_ser_br, ]+;
						[ duzina=?gduzina, sirina=?gsirina, povrs_jed=?gpovrs_jed, marka_mot=?gmarka_mot, ]+;
						[ br_motor=?gbr_motor, god_pro_mot=?ggod_pro_mot, snaga=?gsnaga, marka_pmot=?gmarka_pmot, ]+;
						[ br_sasije=?gbr_sasije, br_pmotor=?gbr_pmotor, god_pro_pmot=?ggod_pro_pmot, ]+;
						[ snaga_pmot=?gsnaga_pmot, koristenje=?gkoristenje, putnika=?gputnika, luka=?gluka, ]+;
						[ luka_zima=?gluka_zima, gran_plov=?ggran_plov, chart_baza=?gchart_baza, ]+;
						[ br_motor2=?gbr_motor2, god_pro_mot2=?ggod_pro_mot2, posada=?gposada, ]+;
						[ dod_nap=?gdod_nap, br_sjeda=?gbr_sjeda, nosivost=?gnosivost, vrsta=?gvrsta ]+;
						[ WHERE proizvod=?xproizvod AND polica=?xbroj])
				ENDIF
			ELSE
				abc_message('UPOZORENJE'+CHR(13)+CHR(13)+'Ne postoje podaci za predmet osiguranja.')
			ENDIF
			USE IN tmpmvprpol
	ENDCASE
	SELECT (ssel)
	RETURN
ENDPROC && prom_mvprpol

*******************************************************************************
* Incubis d.o.o.   	              kreiraj_dokum_pol     (c) All rights reserved
* Author......: Nikola MilkoviÊ                                 Date: 02.09.2015
* Description.: Izrada dokumenata police ako se nisu kreirali
* Dorada......:
*******************************************************************************
PROCEDURE dokum_polpon
	PARAMETERS ppon
	PRIVATE ssel, xtbl
	ssel = SELECT()
	xtbl = IIF(!ppon,stab_pol, stab_pon)
	qInsuranceObject.kreiraj_dokumente_police(broj, proizvod, xtbl)

	SELECT (ssel)
	RETURN .t.
ENDPROC && dokum_pol

*******************************************************************************
* Incubis d.o.o.   	              kreiraj_dokum_pol     (c) All rights reserved
* Author......: Marko Bekafigo                                 Date: 02.09.2015
* Description.: Promjena likvidatora ötete
* Ispravak....: Marko (10.05.2016) - ispravak aûuriranje tablice DODVOZ
*******************************************************************************
FUNCTION promlikstet && promjena likvidatora ötete
PRIVATE ssel, xret, xknj, xbroj, xlikv, xopis, xser
ssel = SELECT()
*
xret = .F.
xknj = knjiga
xbroj = broj
STORE ' ' TO xlikv, xopis
*
IF pgc_ret2val('Promjena likvidatora ötete', 'Likvidator', 'Razlog promjene', 'xlikv', 'xopis', [ext(.F., xlikv, "xlikv", "kor_likv", "sifra", "", "tip='K'")])
	IF !EMPTY(xlikv)
		* Marko - 16.02.2016 - Kreirati zapis u DODVOZ (reg_ozn i marka vozila oöteÊenog)
		PRIVATE ximadv
		ml_sql([SELECT * FROM dodvoz WHERE knjiga=?xknj AND br_prijave=?xbroj INTO CURSOR tmpdv])
		IF _TALLY>0
			ml_sql([UPDATE dodvoz SET sif_likv=?xlikv WHERE knjiga=?xknj AND br_prijave=?xbroj])
		ELSE
			ml_sql([INSERT INTO dodvoz (knjiga, br_prijave, sif_likv) VALUES (?xknj, ?xbroj, ?xlikv)])
		ENDIF
		USE IN tmpdv
		*
		xser = get_ser()
		xopis = LEFT(xopis, 50)
		IF ml_file('stet_likv')
			ml_sql([INSERT INTO stet_likv (serial, knjiga, broj, sif_likv, dat_unos, korisnik, opis) ]+;
				[ VALUES (?xser, ?xknj, ?xbroj, ?xlikv, ?DATE(), ?muser, ?xopis)])
			abc_message('Promjena likvidatora ötete dovröena!')
			xret = .T.
		ENDIF
	ELSE
		abc_message('Neispravna öifra likvidatora!')
	ENDIF
ENDIF
*
SELECT (ssel)
RETURN xret
ENDPROC && promlikstet

*******************************************************************************
* Incubis d.o.o.   	              sred_tar_e            (c) All rights reserved
* Author......: Marko Bekafigo                                 Date: 30.01.2016
* Description.: sred_tar - Dohvat podataka samo po policama src='E'
* Dorada......:
*******************************************************************************
PROCEDURE sred_tar_e
	PRIVATE ssel, xsrce
	ssel = SELECT()
	*
	xsrce = 1
	sred_tar('SRED_TAR4')
	*
	SELECT (ssel)
	RETURN
ENDPROC && sred_tar_e

*******************************************************************************
* Incubis d.o.o.   	              dopis_pon_di          (c) All rights reserved
* Author......: Marko Bekafigo                                 Date: 09.02.2016
* Description.: ispis dopisa po ponudama DI
* Dorada......:
*******************************************************************************
FUNCTION dopis_pon_di
	dok_pon_di('D',.T.)
	RETURN .T.
ENDPROC && dopis_pon_di

*******************************************************************************
* Incubis d.o.o.   	              obav_pon_di           (c) All rights reserved
* Author......: Marko Bekafigo                                 Date: 09.02.2016
* Description.: ispis obavijesti zahvale po ponudama DI
* Dorada......:
*******************************************************************************
FUNCTION obav_pon_di
	dok_pon_di('Z',.T.)
	RETURN
ENDPROC && obav_pon_di

*******************************************************************************
* Incubis d.o.o.   	              obav_pon_di           (c) All rights reserved
* Author......: Marko Bekafigo                                 Date: 09.02.2016
* Description.: ispis obavijesti zahvale po ponudama DI
* Dorada......:
*******************************************************************************
PROCEDURE dok_pon_di
	PARAMETERS ptip, pgru
	PRIVATE ssel, xrec, xbr, xxml, xbroj, xproizvod
	ssel = SELECT()
	*
	IF !((xfqmat_uni OR xfqmat_kva) AND sosn_vrst='02')		&& (ssifra<>'DI' AND proizvod<>'DI') AND (ssifra<>'DT' AND proizvod<>'DT'
		abc_message('UPOZORENJE'+CHR(13)+CHR(13)+'Funkcija dostupna samo za vrstu osiguranja 02')
		RETURN .F.
	ENDIF
	*
	xrec = RECNO()
	xbr = 0
	*
	IF PCOUNT()=2 AND pgru=.T.
		* Grupni tisak dopisa ili obavijesti zahvale za produûenje ponude INAS
		SELECT * FROM ALIAS() INTO CURSOR tmp_pon_di ORDER BY broj
		SCAN ALL
			xbroj = broj
			xproizvod = proizvod
			xxml = qReportsObject.TisakObnoveINAS((xbroj), (xproizvod), '')
			IF !EMPTY(xxml)
				xmltorecordset(xxml)
				IF xbr = 0
					pgc_out(IIF(ptip='D','PRODUIN','OBA_DZI'))
					xbr = 1
				ELSE
					pgc_out(IIF(ptip='D','PRODUIN','OBA_DZI'),,'REPEAT')
				ENDIF
			ENDIF
		ENDSCAN
		USE IN tmp_pon_di
	ELSE
		* PojedinaËni tisak dopisa ili obavijesti zahvale za produûenje ponude INAS
		xbroj = broj
		xproizvod = proizvod
		xxml = qReportsObject.TisakObnoveINAS((xbroj), (xproizvod), '')
		IF !EMPTY(xxml)
			xmltorecordset(xxml)
			pgc_out(IIF(ptip='D','PRODUIN','OBA_DZI'))
		ENDIF
	ENDIF
	*
	SELECT (ssel)
	GOTO xrec
	RETURN .T.
ENDPROC && obav_pon_di


*******************************************************************************
* Incubis d.o.o.   	              dok_pon_di2           (c) All rights reserved
* Author......: Maja JahiÊ	                                   Date: 09.08.2016
* Description.: tisak obavijesti za obnovu INAS police (proizvod DI)
* Dorada......: Marko (12.04.2017) - dorada za proizvod 2B
*******************************************************************************
PROCEDURE dok_pon_di2
	PARAMETERS pgru
	PRIVATE ssel, xrec, xbr, xxml, xbroj, xproizvod, xpolica
	ssel = SELECT()
	*
	xproizvod = proizvod
	IF !INLIST(xproizvod,'DI','DT','2B')	&& (ssifra<>'DI' AND proizvod<>'DI') AND (ssifra<>'DT' AND proizvod<>'DT')
		IF xfqmat_uni
			abc_message('UPOZORENJE'+CHR(13)+CHR(13)+'Funkcija dostupna samo za osigurateljne proizvode DI i 2B')
		ELSE
			abc_message('UPOZORENJE'+CHR(13)+CHR(13)+'Funkcija dostupna samo za osigurateljni proizvod '+IIF(xfqmat_kva, 'DI', 'DT'))
		ENDIF
		RETURN .F.
	ENDIF
	*
	xrec = RECNO()
	xbr = 0
	IF ptip = 'POLICE'
		xpolica = .T.
	ELSE
		xpolica = .F.
	ENDIF
	*
	IF PCOUNT()=1 AND pgru=.T.
		* Grupni tisak za produûenje ponude INAS
		SELECT * FROM ALIAS() INTO CURSOR tmp_pon_di ORDER BY broj
		SCAN ALL
			xbroj = broj
			xproizvod = proizvod
			xxml = qReportsObject.TisakObnoveINAS((xbroj), (xproizvod), '', xpolica)
			IF !EMPTY(xxml)
				xmltorecordset(xxml)
				IF xbr = 0
					pgc_out('PRODUIN2')
					xbr = 1
				ELSE
					pgc_out('PRODUIN2',,'REPEAT')
				ENDIF
			ENDIF
		ENDSCAN
		USE IN tmp_pon_di
	ELSE
		* PojedinaËni tisak za produûenje ponude INAS
		xbroj = broj
		xproizvod = proizvod
		xxml = qReportsObject.TisakObnoveINAS((xbroj), (xproizvod), '', xpolica)
		DO xml_out WITH xxml, 'PRODUIN2'
	ENDIF
	*
	SELECT (ssel)
	GOTO xrec
	RETURN .T.
ENDPROC && dok_pon_di2


*******************************************************************************
* Incubis d.o.o.   	            valid_max_stopa         (c) All rights reserved
* Author......: Marko Bekafigo                                 Date: 14.02.2016
* Description.: validacije maximalnog iznos pop/dop kod ruËnog unosa stope
* Dorada......:
*******************************************************************************
FUNCTION valid_max_stopa
	PARAMETERS pstat_id
	PRIVATE ssel, xret, xmaxstopa
	ssel = SELECT()
	*
	xmaxstopa = ret_ext(pstat_id, 'popdop', '', 'max_izn', 'sifra')
	xret = (xmaxstopa=0 OR get_data1<=xmaxstopa)
	IF xmaxstopa>0 AND get_data1>xmaxstopa
		abc_message('UPOZORENJE'+CHR(13)+CHR(13)+'Maksimalna stopa: '+ALLTRIM(STR(xmaxstopa)))
	ENDIF
	*
	SELECT (ssel)
	RETURN xret
ENDFUNC && valid_max_stopa


*******************************************************************************
* Incubis d.o.o.            valid_max_stopa_dll         (c) All rights reserved
* Author......: Olivera Ilijev-∆ikoviÊ                         Date: 09.05.2017
* Description.: validacije max. iznosa pop/dop kod ruËnog unosa stope
* Dorada......:
*******************************************************************************
FUNCTION valid_max_stopa_dll
	PARAMETERS pstat_id
	PRIVATE ssel, vrati, xosn_vrst
	ssel = SELECT()
	*
	xosn_vrst = ret_xt1('osn_vrst', 'skup_os', 'sifra=?gproizvod', '')
	vrati = qInsuranceObject.valid_max_stopa(pstat_id, xosn_vrst, get_data1)
	IF EMPTY(vrati)
		SELECT (ssel)
		RETURN .T.
	ELSE
		abc_message('UPOZORENJE'+CHR(13)+CHR(13)+vrati)
	ENDIF
	*
	SELECT (ssel)
	RETURN .F.
ENDFUNC && valid_max_stopa_dll


*******************************************************************************
* Incubis d.o.o.                valid_fransiza          (c) All rights reserved
* Author......: Marko Bekafigo 		                           Date: 03.12.2018
* Description.: validacije franûize (ako se mijenja promijeniti i gfransiza)
* Dorada......:
*******************************************************************************
FUNCTION valid_fransiza
	PARAMETERS pget_data1
	IF pget_data1<>gfransiza
		gfransiza = pget_data1
	ENDIF
	RETURN .T.
ENDFUNC && valid_fransiza


*******************************************************************************
* Incubis d.o.o.   	               prom_di1             (c) All rights reserved
* Author......: Marko Bekafigo                                 Date: 03.03.2016
* Description.: Promjena udjela sistematskog pregleda
* Dorada......:
*******************************************************************************
PROCEDURE prom_di1
PRIVATE ssel, xproizvod, xbroj, xvar, xok, xrec, datafstring, xsto
ssel = SELECT()
xrec = RECNO()
*
IF proizvod<>'DI' AND proizvod<>'DT'
	abc_message('UPOZORENJE:'+CHR(13)+CHR(13)+'Akcija dozvoljena samo nad ponudama i policama osigurateljnog proizvoda '+IIF(xfqmat_uni OR xfqmat_kva, 'DI', 'DT'))
	RETURN .F.
ENDIF
*
xsto = 'DI1'
xvar = udio_sist
xproizvod = proizvod
xbroj = broj
xok = .F.
*
datafstring = 'prom_di12'
DO FORM prom_di12 NAME &datafstring
*
IF xok
	ml_sql([UPDATE ]+IIF(ptip = 'POLICE', stab_pol, stab_pon)+[ SET udio_sist=?xvar ]+;
		[ WHERE proizvod=?xproizvod AND broj=?xbroj])
	abc_message('POLICA: '+ALLTRIM(xbroj)+CHR(13)+CHR(13)+'Dovröena promjena udjela sistematskog pregleda ('+ALLTRIM(STR(xvar))+'%)')
ENDIF
*
SELECT (ssel)
GOTO xrec
RETURN .T.
ENDFUNC && prom_di1


*******************************************************************************
* Incubis d.o.o.   	               prom_di2             (c) All rights reserved
* Author......: Marko Bekafigo                                 Date: 03.03.2016
* Description.: Promjena datuma prestanka radnog odonosa
* Dorada......:
*******************************************************************************
PROCEDURE prom_di2
PRIVATE ssel, xproizvod, xbroj, xvar, xok, xrec, datafstring, xsto, xporuka
ssel = SELECT()
xrec = RECNO()
xporuka = ""
*
IF proizvod<>'DI' AND proizvod<>'DT'
	abc_message(pgc_dlang('UPOZORENJE')+CHR(13)+CHR(13)+'Akcija dozvoljena samo nad ponudama i policama osigurateljnog proizvoda '+IIF(xfqmat_uni OR xfqmat_kva, 'DI', 'DT'))
	RETURN .F.
ENDIF
*
IF EMPTY(dat_ver)
	abc_message(pgc_dlang('UPOZORENJE')+CHR(13)+CHR(13)+;
		pgc_dlang('Nedozvoljena akcija')+CHR(13)+CHR(13)+;
		pgc_dlang('Polica nije verificirana'))
	RETURN .F.
ENDIF
*
xsto = 'DI2'
xproizvod = proizvod
xbroj = broj
xok = .F.
*
ml_sql([SELECT prekid_rad FROM ]+IIF(ptip = 'POLICE', 'polosig', 'ponosig')+[ ]+;
	[ WHERE proizvod=?xproizvod AND broj=?xbroj AND rbr_os=1 ]+;
	[ INTO CURSOR tmposig1])
IF _TALLY>0
	xvar = prekid_rad
	*
	datafstring = 'prom_di12'
	DO FORM prom_di12 NAME &datafstring
	*
	IF xok
		*!*	ml_sql([UPDATE ]+IIF(ptip = 'POLICE', 'polosig', 'ponosig')+[ SET prekid_rad=?xvar ]+;
		*!*		[ WHERE proizvod=?xproizvod AND broj=?xbroj AND rbr_os=1])
		xporuka = qInsuranceObject.PrekidRadnogodnosa((xbroj), (xvar))
		abc_message('POLICA: '+ALLTRIM(xbroj)+CHR(13)+CHR(13)+'Dovröena promjena datuma prekida rada ('+DTOC(xvar)+')'+CHR(13)+CHR(13)+xporuka)
	ENDIF
ENDIF
*
SELECT (ssel)
GOTO xrec
RETURN .T.
ENDFUNC && prom_di2


*******************************************************************************
* Incubis d.o.o.   	               prom_fd1             (c) All rights reserved
* Author......: Marko Bekafigo                                 Date: 08.03.2016
* Description.: Promjena datuma dospijeÊa fakture
* Dorada......:
*******************************************************************************
PROCEDURE prom_fd1
PRIVATE ssel, xsto, xbroj, xvar, xok, xrec, datafstring
ssel = SELECT()
*
xrec = RECNO()
xsto = 'FD1'
xbroj = broj
xvar = rok_pla
xok = .F.
*
datafstring = 'prom_di12'
DO FORM prom_di12 NAME &datafstring
*
IF xok
	ml_sql([UPDATE fakture SET rok_pla=?xvar WHERE broj=?xbroj])
	abc_message('FAKTURA: '+ALLTRIM(xbroj)+CHR(13)+CHR(13)+'Dovröena promjena datuma datum dospijeÊa ('+DTOC(xvar)+')')
ENDIF
*
SELECT (ssel)
GOTO xrec
RETURN .T.
ENDFUNC && prom_di2


*******************************************************************************
* Incubis d.o.o.   	              mici_iz_pak           (c) All rights reserved
* Author......: Marko Bekafigo                                 Date: 31.03.2016
* Description.: Akcija za micanje police iz paketa za vanjsku naplatu
* Dorada......:
*******************************************************************************
PROCEDURE mici_iz_pak
PRIVATE ssel, xrec, xpro, xpol, xpak, xpor
ssel = SELECT()
*
xrec = RECNO()
xpro = proizvod
xpol = polica
xpak = br_pak
IF abc_upit('N','éelite li maknuti policu broj '+ALLTRIM(xpol)+' iz vanjske naplate ili paketa?')
	xpor = STRTRAN(qInsuranceObject.Storniraj_polica_va(xpol, xpro, muser, xpak), '</br>')
	IF !EMPTY(xpor)
		abc_message(xpor)
	ENDIF
ENDIF
*
SELECT (ssel)
GOTO xrec
RETURN .T.
ENDFUNC && mici_iz_pak

*******************************************************************************
* Incubis d.o.o.   	          generiraj_paket           (c) All rights reserved
* Author......: Andreas CrnkoviÊ                               Date: 31.03.2016
* Description.: Akcija za generiranje paketa za vanjsku naplatu
* Dorada......:
*******************************************************************************
PROCEDURE generiraj_paket
PRIVATE ssel, xputanja, xpaketfilename, xbroj_paketa, xpor
ssel = SELECT()
*
xputanja = ADDBS(ALLTRIM(unx_not)) + ADDBS(ALLTRIM(muser))
xbroj_paketa = qInsuranceObject.get_broj_paketa()
xpaketfilename = qInsuranceObject.paket_filename(xbroj_paketa)
IF abc_upit('N', 'éelite li generirati paket za vanjsku naplatu?')
	xpor = qInsuranceObject.generiraj_paket(xputanja + xpaketfilename, .T., .T., xbroj_paketa, ALLTRIM(muser))
	IF !EMPTY(xpor)
		abc_message(xpor)
	ELSE
		abc_message("Generiran je Excel " + xputanja + xpaketfilename)
	ENDIF
ENDIF
*
SELECT (ssel)

RETURN .T.
ENDFUNC && generiraj_paket


*******************************************************************************
* Incubis d.o.o.   	            sendmail2incubis        (c) All rights reserved
* Author......: Marko Bekafigo                                 Date: 16.05.2016
* Description.: Slanje maila u In Cubis
* Dorada......:
*******************************************************************************
PROCEDURE sendmail2incubis
	PARAMETERS pfrom, psubject, pbody
	PRIVATE xserver, xuser, xpass
	*
	xserver = ALLTRIM(STRTRAN(PGCENV('MAILSERVER'),'\'))
	xuser = ALLTRIM(STRTRAN(PGCENV('MAILUSER'),'\'))
	xpass = ALLTRIM(STRTRAN(PGCENV('MAILPASSWORD'),'\'))
	*
	QCubisObject.send_mail(pfrom, 'nikola@incubis.hr; marko@incubis.hr; andreas@incubis.hr', '', psubject, pbody, '', xserver, xuser, xpass)
	*
	RETURN
ENDPROC && sendmail2incubis


**********************************************************************
* Naziv:	Punjenje tablice UREPRPOL iz excela
* Poziv:	fill_ureprpol()
* Opis:		()
* Autor:	Zrinko
* Datum:	25.07.2016
**********************************************************************
PROCEDURE fill_ureprpol
PRIVATE ssel, ximp_putanja, ximp_put_done, ximp_put_err, ximp_file, xlist_file, xekstenzija, xok, xserial, xvrsta, xsif_pro, ;
		xid_vrste, xnaz_vrste, datafstring, xpremija, xprem_vrs
*
ssel = SELECT()
pfile = unx_tmp+'fill_ureprpol.txt'
*
SpremiLog(pfile,'*******************************************************')
SpremiLog(pfile, '*')
SpremiLog(pfile, 'PO»ETAK PUNJENJA TABLICE UREPRPOL U '+ DTOC(DATE())+' '+TIME())
SpremiLog(pfile, '*')
*
STORE 0 TO xukup_brslog, xserial
xok=.F.
xlist_file=''
xekstenzija='*.xlsx'
*dio za Ëitanje kljuËeva iz MEGLAINE.INI
upd_env('','URPRPOL_IMPORT')
ximp_putanja=ALLTRIM(pgcenv('URE_IMP_PUT'))
ximp_put_done=LEFT(PGCENV('URE_IMP_PUTD'),LEN(pgcenv('URE_IMP_PUTD'))-1)
ximp_put_err=LEFT(PGCENV('URE_IMP_PUTE'),LEN(pgcenv('URE_IMP_PUTE'))-1)
*
*SET DEFAULT TO (ximp_putanja)
*
xid_vrste='NHR'
xnaz_vrste=RET_EXT(xid_vrste,'URE_VRS',,'OPIS','SIFRA')
datafstring  = 'fill_ureprpol'
DO FORM fill_ureprpol NAME &datafstring

IF xok
	xfile_name = JUSTFNAME(ximp_file)
	xcsv = ximp_putanja+'csv.xlsx'
	IF EMPTY(ximp_file)
		SpremiLog(pfile, 'Import prekinut - korisnik je odustao od importa polica!')
		SpremiLog(pfile, '*')
		SpremiLog(pfile,'ZAVRäETAK OBRADE '+ DTOC(DATE())+' U '+TIME()+'')
		SpremiLog(pfile, '*')
		SET DEFAULT TO (qpoc_dir)
		SELECT (ssel)
		RETURN
	ELSE
		*provjeri ime datoteke
		IF (SUBSTR(xfile_name ,1,3)<>'URE')
			abc_message('Neispravan naziv datoteke!')
			SpremiLog(pfile, 'Import prekinut - neispravan naziv datoteke!Mora poËinjati sa URE')
			SpremiLog(pfile, '*')
			SpremiLog(pfile,'ZAVRäETAK OBRADE '+ DTOC(DATE())+' U '+TIME()+'')
			SpremiLog(pfile, '*')
			*
			IF !EMPTY(xfile_name)
				COPY FILE &ximp_file TO &ximp_put_err
				DELETE FILE &ximp_file RECYCLE
			ENDIF
			*
			SET DEFAULT TO (qpoc_dir)
			SELECT (ssel)
			RETURN
		ENDIF
	ENDIF
	*
	ML_SQL([CREATE CURSOR ccraspakirano ( partner C(50), sif_pro C(5), naknada N(12,2), datum D, cijena N(12,2), tvrtka C(25), model C(25), ]+;
		[ status C(2), sifra C(50), broj_rac C(25), lokacija C(50), serial N(12,0), polica C(13), proizvod C(2), premija N(12,2))])
	*
	*dio za citanje excela, kreiranje CSV tmp filea (korisnik ne vidi jer se temp.csv brise svaki put) i prebacivanje u ccraspakirano
	SELECT ccraspakirano
	oexcel = createobject("excel.application")
	oexcel.workbooks.open(ximp_file)
	oexcel.activeworkbook.saveas(xcsv,39)
	oexcel.activewindow.close(.t.)
	oexcel.quit()
	*
	append from &xcsv TYPE XL5
	*brisi prvi red, ako treba (to je header, ali ako posalje bez headera nece brisati)
	GO TOP
	IF RECCOUNT()>0
		IF UPPER(ALLTRIM(sif_pro))='PROIZ' && briöi prvi red
			DELETE
			SKIP
		ENDIF
	ELSE
		abc_message('Nema podataka za import!')
		SpremiLog(pfile, 'Import prekinut - nema podataka za import!')
		SpremiLog(pfile, '*')
		SpremiLog(pfile,'ZAVRäETAK OBRADE '+ DTOC(DATE())+' U '+TIME()+'')
		SpremiLog(pfile, '*')
		*
		COPY FILE &ximp_file TO &ximp_put_err
		DELETE FILE &ximp_file RECYCLE
		*
		SET DEFAULT TO (qpoc_dir)
		SELECT (ssel)
		RETURN
	ENDIF
	*brisi temp CSV
	DELETE FILE &xcsv RECYCLE
	SET DEFAULT TO (qpoc_dir)
	*eliminirati visak redova (dogaalo se kad reûu podatke iz excela)
	SELECT * FROM ccraspakirano WHERE !EMPTY(partner) INTO CURSOR ccraspakirano READWRITE
	*
	SELECT ccraspakirano
	xukup_brslog=RECCOUNT()
	*
	SCAN ALL
		get_counter('UREPRPOL',xserial)
		REPLACE serial WITH xserial
		REPLACE status WITH ''
		xsif_pro=sif_pro
		*xvrsta='NHR' && za sada
		xprem_vrs=ret_xt1('PREM_VRS', 'URE_SIF', 'sifra=?xsif_pro AND vrsta=?xid_vrste', '')
		xpremija=ret_xt1('PREMIJA', 'URE_SIF', 'sifra=?xsif_pro AND vrsta=?xid_vrste', 0)
		IF ALLTRIM(xprem_vrs)='PO'
			xcijena=cijena
			xpremija=(xpremija/100) * xcijena
		ENDIF
		REPLACE premija WITH xpremija
	ENDSCAN
	*
	SELECT ccraspakirano
	*
	ML_APPEND('ccraspakirano','UREPRPOL') &&tablice polica/ponuda
	*
	abc_message('Importirano je '+ ALLTRIM(STR(xukup_brslog))+ ' slogova u bazu.')
	SpremiLog(pfile, '*')
	SpremiLog(pfile, 'Importirano je '+ ALLTRIM(STR(xukup_brslog))+ ' slogova u bazu.')
	SpremiLog(pfile, '*')
	SpremiLog(pfile,'ZAVRäETAK OBRADE '+ DTOC(DATE())+' U '+TIME()+'')
	SpremiLog(pfile, '*')
	*
	COPY FILE &ximp_file TO &ximp_put_done &&kopira file u folder s obraenim, a nakon toga izbrisati ga iz INPUT foldera
	DELETE FILE &ximp_file RECYCLE
	*
	USE IN ccraspakirano
ENDIF
*
SELECT (ssel)
RETURN
ENDPROC && fill_ureprpol


**********************************************************************
* Naziv:	Generiranje polica iz tablice UREPRPOL
* Poziv:	pol_ureprpol()
* Opis:		()
* Autor:	Zrinko/Nikola
* Datum:	26.07.2016
**********************************************************************
PROCEDURE pol_ureprpol
PRIVATE pfile, xvrs_dat, xvrsta, xod, xdo, xxml, ssel, xok
ssel = SELECT()
*
xok = .t.
*
ml_sql([CREATE CURSOR ccparam( pod D, pdo D, pvrsta C(3))])
*
sG_SEL = SELECT()
pfile = unx_tmp+'punjenje_ureprpol_log.txt'
*
xvrsta = 'NHR'
*
STORE DATE() TO xod, xdo
*
xod = GOMONTH(DATE(YEAR(DATE()),MONTH(DATE()),1),-1)
xdo = DATE(YEAR(DATE()),MONTH(DATE()),1)-1
xxml = 0
DIMENSION avrst(1)
ml_sql([select opis,sifra from ure_vrs into array avrst])
DO FORM poluredaji
SELECT(ssel)
*
IF xok
	SpremiLog(pfile,'*******************************************************')
	SpremiLog(pfile, '*')
	SpremiLog(pfile, 'POKRENUTO PUNJENJE PODATAKA U TABLICU UREPRPOL'+ DTOC(DATE())+' U '+TIME())
	*PGC_RET2VAL('Generiranje polica PLATINUM', 'Datum od', 'Datum do', 'xod', 'xdo')
	PROGRESS_CREATE('*** Punjenje tablice UREPRPOL ***')
	progress.SAY('Prikupljanje kandidata za punjenje')
	*priprema parametara
	ml_sql([INSERT INTO ccparam VALUES (?xod, ?xdo, ?xvrsta)])
	CursorToXML("ccparam","xccparam")
	USE IN ccparam
	*
	progress.SAY('Punjenje tablice UREPRPOL')
	progress.SAY('*** *** *** *** ***')
	progress.SAY('Molimo priËekajte...')
	*
	&&U OVOJ METODI VRATI BROJ SLOGOVA! I PROMJENI POZIV DLL METODE OVO JE DEMO :))
	xxml=qInsuranceObject.kreiraj_police_uredaja(xod, xdo, xvrsta)
	IF xxml>0
		SpremiLog(pfile,'Punjenje podataka zavröeno. Importirano je ' +ALLTRIM(STR(xxml)) + ' polica. ')
	ELSE
		SpremiLog(pfile,'Punjenje podataka zavröeno. Nije importirana niti jedna polica.')
	ENDIF
	progress.SAY('Zavröeno punjenje tablice UREPRPOL. Pogledajte log datoteku za detaljnije informacije.')
	PROGRESS_DESTROY()
	SpremiLog(pfile, 'ZAVRäETAK U '+ DTOC(DATE())+' '+TIME()+'')
	SpremiLog(pfile, '*')
ENDIF
*
RETURN
ENDPROC &&pol_ureprpol

**********************************************************************
* Naziv:	Pregled ureaja po polici
* Poziv:	pregurepol()
* Opis:		()
* Autor:	Nikola
* Datum:	02.08.2016
**********************************************************************
PROCEDURE pregurepol
PRIVATE ssel, xpgc_var, atrfunc_OLD, gridname_OLD, namestring_OLD
ssel = SELECT()
*
xpgc_var = pgc_var('G')
PRIVATE &xpgc_var
STORE '' TO &xpgc_var
IZJ_NUL('IZJ','G','*')
*
ML_SQL(	[ SELECT partner ,lokacija ,sif_pro ,datum ,naknada ,cijena ,tvrtka ,model ,sifra ,broj_rac ,status, serial ,proizvod ,polica   ]+ ;
		[ FROM ureprpol ] + ;
		[ WHERE polica =?gbroj and proizvod =?gproizvod ] + ;
		[ order by datum ] + ;
		[ INTO CURSOR tmure ])
IF _tally > 0
	*
	pgc_grid=.T.
	namestring_OLD = namestring
	gridname = 'grid_odabir'
	namestring = 'ureppol'
	*PUBLIC funcstring
	IF VARTYPE(atrfunc) == 'U'
		atrfunc = ''
	ENDIF
	atrfunc_old = atrfunc
	atrfunc = ''
	DO FORM ureppol NAME &namestring
	atrfunc= atrfunc_old
	pgc_grid=.F.
	namestring = namestring_OLD
	USE IN tmure
	SELECT (SSel)
	DIMENSION  ATR(1), ZAG(1)
	ATR(1)= ''
	ZAG(1)= ''
	load_dd('PRIJAVA', 'ATR', 'POLI09')
	exp_atr('POLI09')
	*
ELSE

	USE IN tmure
	abc_message('Nema ureaja za odabranu policu.')

ENDIF
SELECT (SSel)
RETURN .T.
ENDPROC


**********************************************************************
* Naziv:	Dodijela imena za PDF
* Poziv:	odredi_ime_fajla()
* Opis:		()
* Autor:	Andi/Zrinko
* Datum:	02.08.2016
**********************************************************************
PROCEDURE odredi_ime_fajla
	PRIVATE xlogin
	IF xpdf
		xmerge_dir_local=xtemp_folder+'PROVLIS_'+ALLTRIM(STR(xobracun))+'_'+ALLTRIM(gjmbg)+'.pdf' &&lokacija i naziv lokalnog PDF-a
		xmerge_dir_tmp=xtemp_folder+'TMP_PROVLIS_'+ALLTRIM(STR(xobracun))+'_'+ALLTRIM(gjmbg)+'.pdf' &&lokacija i naziv temp lokalnog PDF-a
		IF FILE(xmerge_dir_local)
			ERASE (xmerge_dir_local)
		ENDIF
		IF FILE(xmerge_dir_tmp)
			ERASE (xmerge_dir_tmp)
		ENDIF
	ENDIF

**********************************************************************
* Naziv:	Ispis i spremanje pdfa
* Poziv:	ispisi_i_spremi_pdf()
* Opis:		()
* Autor:	Andi/Zrinko
* Datum:	02.08.2016
**********************************************************************
PROCEDURE ispisi_i_spremi_pdf
	PARAMETERS preportname, pfilename
	PRIVATE xfilename
	xfilename=ispis_pdf(preportname, pfilename, xtemp_folder)
	IF FILE(xfilename)
		IF FILE(xmerge_dir_local)
			COPY FILE &xmerge_dir_local TO &xmerge_dir_tmp
		ENDIF
		xsuccess=qcubisobject.merge_pdf(xmerge_dir_local,+;
			IIF(FILE(xmerge_dir_tmp),xmerge_dir_tmp,""),+;
			xfilename)
		IF xsuccess
			ERASE (xmerge_dir_tmp)
			ERASE (xfilename)
		ENDIF
	ENDIF
ENDPROC



**********************************************************************
* Naziv:	Ispis i spremanje pdfa2
* Poziv:	ispisi_i_spremi_pdf()
* Opis:		()
* Autor:	Andi/Zrinko
* Datum:	02.08.2016
**********************************************************************
PROCEDURE ispisi_i_spremi_pdf2
	PARAMETERS preportname, pfilename
	PRIVATE xfilename
	xpdfname= pbroj+'_'+IIF(Prafiz='P','pra_','fiz_')+ALLTRIM(DTOC(DATE()))+;
			'_'+SYS(2015)+'.pdf'
	xfilename=ispis_pdf(preportname, xpdfName, xmerge_dir_tmp)
ENDPROC



**********************************************************************
* Naziv:	kopira pdfove u korisnicke notese
* Poziv:	iskopiraj_pdf_u_notese()
* Opis:		()
* Autor:	Andi/Zrinko
* Datum:	02.08.2016
**********************************************************************
PROCEDURE iskopiraj_pdf_u_notese
	PRIVATE xlogin
	IF xpdf
		IF !EMPTY(xputanja) &&ako imaö popunjen parametar iz megaline.ini, onda u tu lokaciju(za sada JAHORINA)
			COPY FILE &xmerge_dir_local TO &xputanja
		ELSE &&u notes korisnika(za sada UNIQA)
			ml_sql("SELECT DISTINCT login FROM SURADNCI WHERE jmbg=?gjmbg AND login<>' ' INTO CURSOR cclogin")
			IF _TALLY>0
				SCAN ALL
					xlogin=ALLTRIM(login)
					ON ERROR terror=.T.
					CD (unx_not+TRIM(xlogin))
					IF terror
						MKDIR (unx_not+TRIM(xlogin))
						MKDIR (unx_not+TRIM(xlogin)+'\TEMP')
						MKDIR (unx_not+TRIM(xlogin)+'\DOC')
						MKDIR (unx_not+TRIM(xlogin)+'\PROFILE')
						COPY FILE (unx_not+'*.*') TO (unx_not+ADDBS(TRIM(xlogin))+'*.*')
					ENDIF
					ON ERROR pgc_error(LINENO())
					CD (terror_lcd)
					xmerge_dir=STRTRAN(xmerge_dir_local, xtemp_folder, unx_not+ADDBS(xlogin))
					COPY FILE &xmerge_dir_local TO &xmerge_dir
				ENDSCAN
			ENDIF
		ENDIF
		ERASE (xmerge_dir_local)
	ENDIF
ENDPROC

*******************************************************************************
* Incubis d.o.o.   	            EXPORT_POD              (c) All rights reserved
* Author......: Danijel »ubraniÊ                               Date: 27.10.2016
* Description.: Export podataka o partnerima u vanjski sustav od
*				Triglav osiguranja
* Dorada......:
*******************************************************************************
PROCEDURE EXPORT_POD
	PRIVATE pputanja
	pputanja=ADDBS(unx_not)+ADDBS(muser)+"partneri.xlsx"
	QInsuranceObject.PartneriExportTriglav("",pputanja)
	ABC_MESSAGE("Partneri su Exportani u "+pputanja)
ENDPROC

********************************************************************************
* Incubis d.o.o.   	            EXPORT_PROV              (c) All rights reserved
* Author......: Danijel »ubraniÊ                                Date: 18.02.2017
* Description.: Export provizijskih lista u vanjski sustav od Triglav osiguranja
* Dorada......:
********************************************************************************
PROCEDURE EXPORT_PROV
	PRIVATE pputanja,xbroj
	xbroj=broj
	pputanja=ADDBS(unx_not)+ADDBS(muser)+"provizije.xlsx"
* parametri metode u dll
* datum od,datum do, öifra podruûnice ili * iz tablice prodstru,öifra prodajnog
* kanala ili * iz tablice prodstru
	QInsuranceObject.ProvizijeExportTriglav(xbroj,"*","*",pputanja)
	ABC_MESSAGE("Provizije su Exportane u "+pputanja)
ENDPROC


***************************************************************
* Naziv:	Poniötenje kartice DZO (nad karticom)
* Poziv:	PONIST_DZKAR()
* Opis:		()
* Autor:	Nikola MilkoviÊ
* Datum:	09.11.2016
***************************************************************
PROCEDURE PONIST_DZKAR
PRIVATE ssel, xbr_kart
ssel=SELECT()
*
xbr_kart=ALLTRIM(br_kart)
IF abc_upit('Y','éelite li poniötiti izdanu karticu ('+ALLTRIM(xbr_kart)+') ?')

	upd_env('','DZO_ISKAZNICE')
	qInsuranceObject.ponisti_kar(xbr_kart)
ENDIF
SELECT(ssel)
ENDPROC

***************************************************************
* Naziv:	Otkaz kartice DZO (nad karticom)
* Poziv:	OTKAZ_DZKAR()
* Opis:		()
* Autor:	Nikola MilkoviÊ
* Datum:	11.11.2016
***************************************************************
PROCEDURE OTKAZ_DZKAR
PRIVATE ssel, xbr_kart, xdatum, xdat_poc, xdat_ist
ssel=SELECT()
*
xbr_kart=ALLTRIM(br_kart)
xstatus = status
IF UPPER(LEFT(xstatus ,1))== 'S'
	ABC_MESSAGE('Kartica je stornirana.')
	RETURN
ENDIF

IF abc_upit('Y','éelite li OTKAZATI izdanu karticu ('+ALLTRIM(xbr_kart)+') ?')
	xdatum = DATE()
	xdat_poc = dat_poc
	xdat_ist = dat_ist
	xdatum =PGC_RETVAL('Unesite datum otkaza:',xdatum )
	IF xdatum > xdat_poc  and  xdatum <xdat_ist

		upd_env('','DZO_ISKAZNICE')

		qInsuranceObject.storno_kart(xbr_kart, STRTRAN(PGCENV('DZO_KART_SO'),'\'),xdatum   )
	ELSE
		ABC_MESSAGE('Datum koji ste unijeli je neispravan.')
	ENDIF
ENDIF
SELECT(ssel)
ENDPROC

*******************************************************************************
PROCEDURE TN_ZATVAR && ZATVARANJE TRAJNIH NALOGA (TRIGLAV)
DO TN_OTVAR WITH 'Z' IN TN_OTVAR
RETURN
ENDPROC && TN_ZATVAR

********************************************************************************
* Incubis d.o.o.              user_excel                 (c) All rights reserved
* Author......: Andreas CrnkoviÊ                                Date: 21.02.2017
* Description.: Import usera i suradnika iz Excela
* Dorada......:
********************************************************************************
PROCEDURE user_excel
	PRIVATE xpath
	xpath='                                                                                                                          '
	xpath=PGC_RETVAL('Upiöite putanju do Excel dokumenta.', xpath)
	IF FILE(xpath)
		qInsuranceObject.insertusersura_from_excel(xpath)
	ELSE
		ABC_MESSAGE('Upisali ste nepostojeÊu datoteku.')
	ENDIF
	RETURN
ENDPROC && user_sura_excel

********************************************************************************
PROCEDURE TN_PDFUPL && UPLATNICE PO NEZADUéENIM TRAJNIM NALOZIMA (TRIGLAV)
DO TN_IMPORT WITH 'P' IN TN_IMPORT
RETURN
ENDPROC && TN_PDFUPL


***************************************************************************************************
* Naziv:	Punjenje tablice DZO_KART podacima o policama s ugovorenim rizicima DZO
* Poziv:	fill_pon1e()
* Opis:		()
* Autor:	Zrinko PuljiÊ
* Datum:	11.03.2017
***************************************************************************************************
PROCEDURE fill_pon1e
PRIVATE pfile, ximp_file, xxml
*
pfile = unx_tmp+'Log_pon1E.txt'
*
SpremiLog(pfile,'*******************************************************')
SpremiLog(pfile, '*')
SpremiLog(pfile, 'POKRENUTO KREIRANJE PONUDA PROIZVODA 1E '+ DTOC(DATE())+' U '+TIME())
SpremiLog(pfile, '*')
ximp_file = GETFILE('xlsx','','',0,'Datoteka s ulaznim podacima')
PROGRESS_CREATE('*** Kreiranje ponuda 1E ***')
progress.SAY('Prikupljanje kandidata za punjenje')
*
progress.SAY('*** *** *** *** ***')
progress.SAY('Molimo priËekajte...')
*
xxml=qInsuranceObject.PonudeIzPolica1E(ximp_file)
abc_message(xxml)
progress.SAY('Zavröeno punjenje ponuda 1E. Pogledajte log datoteku za detaljnije informacije.')
PROGRESS_DESTROY()
SpremiLog(pfile, 'ZAVRäETAK U '+ DTOC(DATE())+' '+TIME()+'')
SpremiLog(pfile, '*')
*
RETURN
ENDPROC && fill_pon1e


**
PROCEDURE UNX_UNLOCK
	PARAMETERS pkljuc, pobjekt
	qCubisObject.Unlock(pkljuc, pobjekt)
ENDPROC


********************************************************************************
PROCEDURE ULAZI_SB
PARAMETERS UniFil, PovName
DO SB IN ulazi_se WITH UniFil, PovName
RETURN
ENDPROC

*******************************************************************************
* oli wiener popuni grupe paketa  (21.03.2017)
*******************************************************************************
PROCEDURE popuni_pakete
PRIVATE ssel, xsifre
ssel=SELECT()

PRIVATE crostart,eurodin
crostart=Qcrostart   &&['180401','180403','180404','180501','180504']
eurodin=Qeurodin  &&['180402','180403','180502']
*!*	crostart=[('180401','180403','180404','180501','180504')]
*!*	eurodin=[('180402','180403','180502')]
* paket 7
ml_sql([select broj,'7' as paket from  poli10 where  ao_paket=' ' and broj in  (select broj from st_pol where ALLTRIM(stat_id)='180507') and broj in (select broj from st_pol where ALLTRIM(stat_Id) in ]+crostart+[) and broj not in (select broj from pol_aopak) into cursor tmpaaa])
ABC_WAIT('Aûuriram paket 7 ... Pronaeno '+STR(_tally)+' polica ...' )
ml_append('tmpaaa','pol_aopak')
USE IN tmpaaa
*
ml_sql([select  broj,'8' as paket from  poli10  where   ao_paket=' ' and broj in  (select broj from st_pol where ALLTRIM(stat_id)='180509') and broj in (select broj from st_pol where ALLTRIM(stat_Id) in ]+crostart+[) and broj not in (select broj from pol_aopak) into cursor tmpaaa])
ABC_WAIT('Aûuriram paket 8 ... Pronaeno '+STR(_tally)+' polica ...' )
ml_append('tmpaaa','pol_aopak')
USE IN tmpaaa
*
ml_sql([select  broj,'9' as paket from poli10  where   ao_paket=' ' and broj in  (select broj from st_pol where ALLTRIM(stat_id)='180507') and broj in (select broj from st_pol where ALLTRIM(stat_Id) in ]+eurodin+[) and broj not in (select broj from pol_aopak) into cursor tmpaaa])
ABC_WAIT('Aûuriram paket 9 ... Pronaeno '+STR(_tally)+' polica ...' )
ml_append('tmpaaa','pol_aopak')
USE IN tmpaaa
*
ml_sql([select  broj,'10' as paket from poli10 where   ao_paket=' ' and broj in (select broj from st_pol where ALLTRIM(stat_id)='180509') and broj in (select broj from st_pol where ALLTRIM(stat_Id) in ]+eurodin+[) and broj not in (select broj from pol_aopak) into cursor tmpaaa])
ABC_WAIT('Aûuriram paket 10 ... Pronaeno '+STR(_tally)+' polica ...' )
ml_append('tmpaaa','pol_aopak')
USE IN tmpaaa
*
ml_sql([select  broj,'11' as paket from poli10  where   broj in  (select broj from st_pol where ALLTRIM(stat_id)='180507') and broj not in (select broj from st_pol where ALLTRIM(stat_Id) in ]+crostart+[ or ALLTRIM(stat_id) in ]+eurodin+[) and ao_paket='4' and broj not in (select broj from pol_aopak) into cursor tmpaaa])
ABC_WAIT('Aûuriram paket 11 ... Pronaeno '+STR(_tally)+' polica ...' )
ml_append('tmpaaa','pol_aopak')
USE IN tmpaaa
*
ml_sql([select  broj,'12' as paket from poli10 where  broj in  (select broj from st_pol where ALLTRIM(stat_id)='180509') and broj not in (select broj from st_pol where ALLTRIM(stat_Id) in ]+crostart+[ or ALLTRIM(stat_id) in ]+eurodin+[) and ao_paket='4' and broj not in (select broj from pol_aopak) into cursor tmpaaa])
ABC_WAIT('Aûuriram paket 12 ... Pronaeno '+STR(_tally)+' polica ...' )
ml_append('tmpaaa','pol_aopak')
USE IN tmpaaa
*
ml_sql([select  broj,'13' as paket from poli10 where  broj in  (select broj from st_pol where ALLTRIM(stat_id)='180507') and broj in (select broj from st_pol where ALLTRIM(stat_Id) in ]+crostart+[) and ao_paket='1' and broj not in (select broj from pol_aopak) into cursor tmpaaa])
ABC_WAIT('Aûuriram paket 13 ... Pronaeno '+STR(_tally)+' polica ...' )
ml_append('tmpaaa','pol_aopak')
USE IN tmpaaa
*
ml_sql([select  broj,'14' as paket from poli10 where  broj in  (select broj from st_pol where ALLTRIM(stat_id)='180507') and broj in (select broj from st_pol where ALLTRIM(stat_Id) in ]+crostart+[) and ao_paket='2' and broj not in (select broj from pol_aopak) into cursor tmpaaa])
ABC_WAIT('Aûuriram paket 14 ... Pronaeno '+STR(_tally)+' polica ...' )
ml_append('tmpaaa','pol_aopak')
USE IN tmpaaa
*
ml_sql([select  broj,'15' as paket from poli10 where broj in  (select broj from st_pol where ALLTRIM(stat_id)='180509') and broj in (select broj from st_pol where ALLTRIM(stat_Id) in ]+crostart+[) and ao_paket='1' and broj not in (select broj from pol_aopak) into cursor tmpaaa])
ABC_WAIT('Aûuriram paket 15 ... Pronaeno '+STR(_tally)+' polica ...' )
ml_append('tmpaaa','pol_aopak')
USE IN tmpaaa
*
ml_sql([select  broj,'16' as paket from poli10 where  broj in  (select broj from st_pol where ALLTRIM(stat_id)='180509') and broj in (select broj from st_pol where ALLTRIM(stat_Id) in ]+crostart+[) and ao_paket='2' and broj not in (select broj from pol_aopak) into cursor tmpaaa])
ABC_WAIT('Aûuriram paket 16 ... Pronaeno '+STR(_tally)+' polica ...' )
ml_append('tmpaaa','pol_aopak')
USE IN tmpaaa
*
ml_sql([select  broj,'17' as paket from poli10 where broj in  (select broj from st_pol where ALLTRIM(stat_id)='180507') and broj in (select broj from st_pol where ALLTRIM(stat_Id) in ]+eurodin+[)  and ao_paket='1' and broj not in (select broj from pol_aopak)  into cursor tmpaaa])
ABC_WAIT('Aûuriram paket 17 ... Pronaeno '+STR(_tally)+' polica ...' )
ml_append('tmpaaa','pol_aopak')
USE IN tmpaaa
*
ml_sql([select broj,'18' as paket from poli10 where  broj in  (select broj from st_pol where ALLTRIM(stat_id)='180507') and broj in (select broj from st_pol where ALLTRIM(stat_Id) in ]+eurodin+[)  and ao_paket='2'  and broj not in (select broj from pol_aopak) into cursor tmpaaa])
ABC_WAIT('Aûuriram paket 18 ... Pronaeno '+STR(_tally)+' polica ...' )
ml_append('tmpaaa','pol_aopak')
USE IN tmpaaa
*
ml_sql([select  broj,'19' as paket from poli10 where   broj in  (select broj from st_pol where ALLTRIM(stat_id)='180509') and broj in (select broj from st_pol where ALLTRIM(stat_Id) in ]+eurodin+[) and ao_paket='1' and broj not in (select broj from pol_aopak)  into cursor tmpaaa])
ABC_WAIT('Aûuriram paket 19 ... Pronaeno '+STR(_tally)+' polica ...' )
ml_append('tmpaaa','pol_aopak')
USE IN tmpaaa
*
ml_sql([select broj,'20' as paket from poli10 where   broj in  (select broj from st_pol where ALLTRIM(stat_id)='180509') and broj in (select broj from st_pol where ALLTRIM(stat_Id) in ]+eurodin+[) and ao_paket='2'  and broj not in (select broj from pol_aopak) into cursor tmpaaa])
ABC_WAIT('Aûuriram paket 20 ... Pronaeno '+STR(_tally)+' polica ...' )
ml_append('tmpaaa','pol_aopak')
USE IN tmpaaa
*
ml_sql([select broj,'21' as paket from poli10 where   broj in  (select broj from st_pol where ALLTRIM(stat_id)='180510') and broj not in (select broj from pol_aopak) into cursor tmpaaa])
ABC_WAIT('Aûuriram paket 21 ... Pronaeno '+STR(_tally)+' polica ...' )
ml_append('tmpaaa','pol_aopak')
USE IN tmpaaa
*
ml_sql([select  broj,ao_paket as paket from poli10 where ao_paket in ('1','2','4')  and broj not in (select broj from pol_aopak) into cursor tmpaaa])   && na kraju popuni öto je ostalo prazno
ABC_WAIT('Aûuriram paket 1,2 i 4 ... Pronaeno '+STR(_tally)+' polica ...' )
ml_append('tmpaaa','pol_aopak')
USE IN tmpaaa
* punjenje nakon izdane police
SELECT(ssel)
RETURN


***********************
PROCEDURE kojipaket
***********************
PARAMETERS dajporuku
PRIVATE xbroj,xao_paket
PRIVATE ssel, vrati, xasis, xcro, xeuro, xpak_riz, xanova
ssel = SELECT()
*
IF PCOUNT()=0
	dajporuku = .T.
ENDIF
*
IF dajporuku
	xbroj = broj
	xao_paket = ao_paket
ELSE
	xbroj = gbroj
	xao_paket = gao_paket
ENDIF
ml_sql('SELECT paket FROM pol_aopak WHERE broj=?xbroj INTO CURSOR tmpdajpak')
IF _TALLY>0
	xpak_riz = paket
ENDIF
USE IN tmpdajpak
*
STORE '' TO vrati, xasis, xcro, xeuro,  xanova
PRIVATE crostart, eurodin
crostart = qcrostart
eurodin = qeurodin
*
IF !EMPTY(crostart)
	ml_sql([SELECT stat_id FROM st_pol WHERE (stat_id='180507' OR stat_id='180509' OR stat_id='180510' OR stat_id='180505' OR stat_id='180506' OR stat_id='180511' OR stat_id='180512') AND broj=?xbroj INTO CURSOR tmpasis])
	IF _TALLY>0
		xasis = stat_id
	ENDIF
	USE IN tmpasis

	* pronai CROSTART
	ml_sql([SELECT stat_id FROM st_pol WHERE ALLTRIM(stat_id) in ]+crostart+[ and broj=?xbroj INTO CURSOR tmpcro])
	IF _TALLY>0
		xcro = stat_id
	ENDIF
	USE IN tmpcro

	* pronai EURODIN
	ml_sql([SELECT stat_id FROM st_pol WHERE ALLTRIM(stat_id) in ]+eurodin+[ and broj=?xbroj INTO CURSOR tmpeuro])
	IF _TALLY>0
		xeuro = stat_id
	ENDIF 
	USE IN tmpeuro

	ml_sql([SELECT stat_id FROM st_pol WHERE (stat_id='180511' or stat_id='180512') and broj=?xbroj INTO CURSOR tmpanova])
	IF _TALLY>0
		xanova = stat_id
	ENDIF
	USE IN tmpanova

	DO CASE
		CASE xanova='180511'
			vrati = '24'		
		CASE EMPTY(xao_paket) AND xasis='180507' and !EMPTY(xcro)
			vrati = '7'
		CASE EMPTY(xao_paket) AND xasis='180509' and !EMPTY(xcro)
			vrati = '8'
		CASE EMPTY(xao_paket) AND xasis='180507' and !EMPTY(xeuro)
			vrati = '9'
		CASE EMPTY(xao_paket) AND xasis='180509' and !EMPTY(xeuro)
			vrati = '10'
		CASE xao_paket='4' AND xasis='180507' AND EMPTY(xcro) and EMPTY(xeuro)
			vrati = '11'
		CASE xao_paket='4' AND xasis='180509' AND EMPTY(xcro) and EMPTY(xeuro)
			vrati = '12'
		CASE xao_paket='1' AND xasis='180507' AND !EMPTY(xcro) and EMPTY(xeuro)
			vrati = '13'
		CASE xao_paket='2' AND xasis='180507' AND !EMPTY(xcro) and EMPTY(xeuro)
			vrati = '14'
		CASE xao_paket='1' AND xasis='180509' AND !EMPTY(xcro) and EMPTY(xeuro)
			vrati = '15'
		CASE xao_paket='2' AND xasis='180509' AND !EMPTY(xcro) and EMPTY(xeuro)
			vrati = '16'
		CASE xao_paket='1' AND xasis='180507' AND EMPTY(xcro) AND !EMPTY(xeuro)
			vrati = '17'
		CASE xao_paket='2' AND xasis='180507' AND EMPTY(xcro) and !EMPTY(xeuro)
			vrati = '18'
		CASE xao_paket='1' AND xasis='180509' AND EMPTY(xcro) AND !EMPTY(xeuro)
			vrati = '19'
		CASE xao_paket='2' AND xasis='180509' AND EMPTY(xcro) and !EMPTY(xeuro)
			vrati = '20'
		CASE xasis='180510'     && nova akcija auto asistencija 0 kuna &&AND EMPTY(xcro) and !EMPTY(xeuro)
			vrati = '21'
		CASE xasis='180505'
			vrati = '22'
		CASE xasis='180506'
			vrati = '23'
		CASE !EMPTY(xao_paket) AND EMPTY(xasis) AND EMPTY(xcro) and EMPTY(xeuro) and EMPTY(xanova)
			vrati = xao_paket
	ENDCASE
	IF dajporuku 
		IF !EMPTY(vrati)
			abc_message(pgc_dlang('Paket')+': '+vrati+CHR(13)+CHR(13)+;
				pgc_dlang('U bazi zapisano')+': '+xpak_riz)
		ELSE
			abc_message(pgc_dlang('Paket')+':')
		ENDIF
	ENDIF
ENDIF
*
SELECT (ssel)
RETURN vrati


PROCEDURE Storno3f_new
PRIVATE SSel,XFile, xdat_30, xdat_90, xbez_stat_ziv, xuvjstat
* ---
SSel=SELECT()
* traûenje rata DRUGIH OPOMENA i kasnijih fakturiranih rata tih polica
WAIT WINDOW 'DohvaÊam podatke' NOWAIT
xdat_30 = DATE()-30
xdat_90 = DATE()-90
xuvjstat  = ""
IF fqmat_broj('UNIQA')
	xbez_stat_ziv = "'" + STRTRAN(STRTRAN(PGCENV('OPOBEZ_STATZIV'), '\'),";","','") + "'"
	xuvjstat = " and p.id_status not in ( " + xbez_stat_ziv + " ) "
ENDIF

IF ML_SQL([ SELECT DISTINCT s.serial, s.polica, s.rata, s.faktura, ] + ;
	[ s.dat_obr as dat_dosp, s.iznos - s.iznos_s as iznos,s.iznosp - s.iznos_s as placeno, p.ugo_id, p.ugo_naz  ]+ ;
	[ FROM fakture f, st_prem t, st_prem s, st_opomp sp, opomenep op, police_all p ]+ ;
	[ WHERE f.broj=t.faktura AND t.polica=s.polica AND ]+ ;
		[ p.broj = t.polica AND ] + ;
		[ sp.serial_r=t.serial AND sp.broj=op.broj AND ]+ ;
		[ op.stupanj=2 AND t.rata<=s.rata AND ]+ ;
		[ f.tip IN ('V','D','U') AND op.dat_obr<=?xdat_30 AND ]+ ;
		[ t.iznosp<(t.iznos*0.75) AND s.faktura<>SPACE(8) AND ]+ ;
		[ t.dat_obr <= ?xdat_90 AND ] + ;
		[ f.datum>=?DATE(2017,01,01) ] + ;
		xuvjstat + ;
	[ ORDER BY s.polica, s.rata ]+ ;
	[ INTO CURSOR t_1]) && poredano za kasnije spremanje rezultata

	IF _TALLY>0 AND !ISNULL(serial) AND VARTYPE(serial)=='N'
		* storniranje faktura
		WAIT WINDOW 'Storniram rate ' NOWAIT
		IF ML_SQL([SELECT DISTINCT faktura FROM t_1 INTO CURSOR t_2])
			SCAN ALL
				PUNI_LOG((t_2.faktura),,'U5','FAKTURE')
				IF !ML_SQL([UPDATE fakture SET stat_fak='9' WHERE broj=']+t_2.faktura+[']) OR ;
					!Ok_anu_ra(t_2.faktura) && anuliranje rata
					EXIT
				ENDIF
			ENDSCAN
			USE IN t_2
		ENDIF
		* spremanje rezultata
		xds = loDotNetBridge.cursortodataset('t_1')
		IF VARTYPE(xds)='O'
			xfile = 'Storno3F_new' + '_' + TTOC(DATETIME(),1) + '.xlsx'
			qCubisObject.WriteXlsx_ds(xds, unx_not+ADDBS(TRIM(muser)) + xfile)
			abc_message('U vaöem notes folderu nalazi se ' + xfile + ' datoteka s rezultatima.')
			TRY
				oexcel=CREATEOBJECT("Excel.Application")
				oexcel.VISIBLE=.T.
				ON ERROR abc_error(0,'Excel datoteka nije kreirana...')
				oexcel.workbooks.OPEN(unx_not+ADDBS(TRIM(muser)) + xfile)
			CATCH
				*
			ENDTRY
		ELSE
			abc_message('UPOZORENJE'+CHR(13)+CHR(13)+'Greöka u pripremi podataka')
		ENDIF
	ENDIF
	USE IN t_1
ELSE
	WAIT WINDOW 'Pogreöka prilikom traûenja podataka o stavkama raËuna!'
ENDIF
* zakljuËak
WAIT WINDOW 'ok' TIMEOUT 1
SELECT (SSel)
RETURN


***************************************************************
* Naziv:	Automatsko prebacivanja sredstava web naplate u CUBIS
* Poziv:	DO IMP_WEBSRE ili IMP_WEBSRE()
* Autor:	Zrinko
* Datum:	24.04.2011.
* Promjene: Zrinko je autor samo poziva da se moûe pokrenuti iz Wincubisa.
*			Samu logiku je pisao Jadran
***************************************************************
PROCEDURE imp_websre
PRIVATE SSel
SSel=SELECT()
*
qInsuranceObject.UpdateCekPlaid()
qInsuranceObject.InsertCek(.T.)
*
SELECT (SSel)
RETURN
ENDPROC && imp_websre


***************************************************************
* Naziv:	Storno dzo obrade po autorizaciji
* Poziv:	STDZAUTO()
* Autor:	Nikola MilkoviÊ
* Datum:	24.05.2017.
* Promjene:
***************************************************************
FUNCTION STDZAUTO
	DO storno_dzo IN dzo_racun WITH br_rac
ENDFUNC

***************************************************************
* Naziv:	Ok_anu_ra - prebaËeno iz storno3f
* Poziv:	Ok_anu_ra ()
* Autor:	Nikola MilkoviÊ
* Datum:	30.05.2017.
* Promjene:
***************************************************************
FUNCTION Ok_anu_ra && anuliranje rata (code refinement)
PARAMETERS PFaktura
PRIVATE XOk,XSerial
XOk=.F.
IF ML_SQL([SELECT DISTINCT serial FROM st_prem WHERE faktura=']+PFaktura+[' INTO CURSOR t_oar])
	IF _TALLY>0 AND !ISNULL(serial) AND VARTYPE(serial)=='N'
		XOk=.T.
		SCAN ALL
			XOk=.F.
			XSerial=t_oar.serial
			IF !ML_SQL([UPDATE st_prem SET faktura=']+SPACE(8)+[' WHERE serial=?XSerial]) OR ;
				!ML_SQL([UPDATE st_prem SET iznos=0 WHERE serial=?XSerial AND iznosp<iznos AND rata>1])
				EXIT
			ENDIF
			XOk=.T.
		ENDSCAN
	ENDIF
	USE IN t_oar
ENDIF
RETURN XOk
ENDFUNC && Ok_anu_ra


***************************************************************
* Naziv:	storno_ure
* Poziv:	storno_ure()
* Autor:	Nikola MilkoviÊ
* Datum:	16.06.2017.
* Promjene:
***************************************************************
FUNCTION storno_ure&& anuliranje rata (code refinement)
PARAMETERS pserial

IF ABC_UPIT('Y','Jeste li sigurni da ûelite stornirati ureaj?')
	QInsuranceObject.StornirajUredjajElipso(pserial)
ENDIF
ENDFUNC


***************************************************************
* Naziv:	part_nek
* Poziv:	part_nek()
* Autor:	Nikola MilkoviÊ
* Datum:	26.06.2017.
* Promjene:
***************************************************************
FUNCTION part_nek&&
PARAMETERS pponovo
IF ABC_UPIT('Y','Jeste li sigurni da ûelite pokrenuti proceduru nekoriötenih partnera?')
	QCUBISOBJECT.NekoristeniPartneri(pponovo)
ENDIF
ENDFUNC

*************************************************************************************************
* InCubis d.o.o.              UPD_POZBR	            				   (C) All rights reserved
* Author......: Zrinko PuljiÊ		                        				  Date: 19.07.2017.
* Description.: Promjena poziva na broj nad izvodom Uniqa
*************************************************************************************************
PROCEDURE upd_pozbr
PRIVATE ssel, xpoz_br, xpoz_br_new, xsektor, xsvrha_new, xserial
ssel = SELECT()
*
xpoz_br = poziv_br
xpoz_br_new = ''
xserial = serial
xsektor = ret_xt1('sektor', 'fakture', [broj=']+xpoz_br +['], '0')
xsvrha_new = STRTRAN(pgcenv('SVRHA_KONVERZ'),'\')
*
IF EMPTY(ALLTRIM(xsvrha_new))
	abc_message('Nedostaje kljuË u MEGALINE.INI!')
	RETURN
ENDIF
*
IF xsektor='1'
	ml_sql([SELECT DISTINCT polica FROM st_prem WHERE faktura=?xpoz_br INTO CURSOR ccst_prem])
	*
	IF _TALLY=0
		abc_message('Konverzija nije moguÊa jer poziv na broj nije faktura!')
	ENDIF
	*
	IF _TALLY=1&&sluËaj kad je konverzija moguÊa
		IF abc_upit('Y','éelite li konvertirati poziv na broj?')
			xpoz_br_new = polica
			ml_sql([UPDATE st_izvod SET poziv_br=?xpoz_br_new, svrha=?xsvrha_new WHERE serial=?xserial])
			abc_message('Poziv na broj je konvertiran!')
		ELSE
			abc_message('Poziv na broj nije konvertiran!')
		ENDIF
	ENDIF
	*
	IF _TALLY>1&&potrebna ruËna razrada
		abc_message('Konverzija nije moguÊa! Potrebna ruËna razrada izvoda.')
	ENDIF
	*
	USE IN ccst_prem
ELSE && za pravni se ne radi
	IF xsektor='2'
		abc_message('Konverzija nije moguÊa jer je faktura pravnog sektora!')
	ELSE
		abc_message('Konverzija nije moguÊa jer poziv na broj nije faktura!')
	ENDIF
ENDIF
*
SELECT (ssel)
ENDPROC && upd_pozbr


*************************************************************************************************
* InCubis d.o.o.              GET_USERPASS	            				   (C) All rights reserved
* Author......: Zrinko PuljiÊ		                        				  Date: 08.08.2017.
* Description.: Kreiraju se passwordi
*************************************************************************************************
PROCEDURE get_userpass
PRIVATE ssel, ximp_file
ssel = SELECT()
*
ximp_file = GETFILE('xslx','','',0,'Excel datoteka s korisnicima')
IF !EMPTY(ximp_file)
	QCubisObject.GetUsersPass(ximp_file)
	abc_message('Aûurirana datoteka se nalazi u notes folderu korisnika!')
ENDIF
SET DEFAULT TO (qpoc_dir)
*
SELECT (ssel)
ENDPROC && get_userpass


*******************************************************************************
* Marko (22.04.2016) - kalkulacija sintetike iz stavki valutne police
*******************************************************************************
FUNCTION ret_tipreosig
PARAMETERS pid_vrsreug
PRIVATE ssel, xret
ssel = SELECT()
*
IF EMPTY(pid_vrsreug)
	RETURN ''
ENDIF
*
xret = ''
ml_sql([SELECT * FROM vrs_re_ug WHERE sifra=?pid_vrsreug INTO CURSOR tmpvrs_re_ug])
IF _TALLY>0
	IF prop_ind=1
		xret = 'P'
	ELSE
		xret = 'N'
	ENDIF
ENDIF
USE IN tmpvrs_re_ug
*
SELECT (ssel)
RETURN xret
ENDFUNC && ret_tipreosig


*******************************************************************************
* Marko (19.09.2017) - prebaËeno iz komerc.prg, komentirano u popdop_br.prg
*******************************************************************************
FUNCTION pro_sto
PRIVATE xstop
DO CASE
	CASE snac_prem=='1' && promil
		xstop = 1000
	CASE snac_prem=='2' && postotak
		xstop = 100
	CASE snac_prem=='3' && koeficijent
		xstop = 1
	OTHERWISE && vraÊa neizmjenjenu stopu
		xstop = 1
ENDCASE
REPLACE	ukupno WITH premija, stopa WITH ROUND(premija*xstop/osnov, 4)
xiznos = osnov + IIF(operac=='-', -ukupno, ukupno)
RETURN .T.
ENDFUNC && pro_sto


*******************************************************************************
* Marko (25.10.2017) - kontrola ponude DZO u procesu policiranja (POLIC_PON)
* Marko (26.02.2018) - poziv dll metode za provjeru ponude DZO
*******************************************************************************
FUNCTION ctrl_polpon_dzo
PARAMETERS pds_ctrl, pproizvod, pbr_pon, pporuka
PRIVATE ssel, xret
ssel = SELECT()
*
xret = qInsuranceObject.ctrl_polpon_dzo(pds_ctrl, pproizvod, pbr_pon, pporuka)
*
SELECT (ssel)
RETURN xret
ENDFUNC && ctrl_polpon_dzo


*******************************************************************************
PROCEDURE zkbiro_prip1
ZKBIRO_PRIP(.T.)
RETURN
ENDFUNC && zkbiro_prip1


*******************************************************************************
FUNCTION upd_status
PARAMETERS ptablica, pproizvod, pstatus, pdana, popis, pexcel_out
PRIVATE ssel, xret, xrbr, xlast, xtext, xdatum, xtablica, xtab_stat, xbroj, xopis, xexcel_out, xpuni_log
ssel = SELECT()
*
xret = 0
xdatum = DATE()-pdana
xtablica = ALLTRIM(ret_xt1(IIF(ptablica='POL','tab_pol','tab_pon'), [skup_os], [sifra=']+pproizvod+['], ''))
xtab_stat = IIF(ptablica=='POL','stat_pol','stat_pon')
xopis = IIF(PCOUNT()<5, ' ', popis)
xexcel_out = IIF(PCOUNT()<6, .T., pexcel_out)
xpuni_log = (VARTYPE(xid_log)='C' AND !EMPTY(xid_log))
*
IF !EMPTY(xtablica)
	ml_sql([SELECT proizvod, broj, datum, id_status, ?pdana AS broj_dana, ?pstatus AS storno_status ]+;
		[ FROM ]+xtablica+[ ]+;
		[ WHERE proizvod=?pproizvod AND datum<=?xdatum AND id_status=' ' ]+;
		[ INTO CURSOR tmp1])
	xret = (_TALLY)
	xrbr = 0
	xlast = RECCOUNT()
	xtext = ''
	SCAN ALL
		xbroj = broj
		*
		xrbr = xrbr + 1
		xtext = pproizvod+' ('+ALLTRIM(xbroj)+')'
		pgc_bls(xrbr/xlast, .F., xtext)
		*
		* update id_status ponude
		IF ml_sql([UPDATE ]+xtablica+[ SET id_status=?pstatus WHERE proizvod=?pproizvod AND broj=?xbroj AND datum<=?xdatum AND id_status=' '])
			*
			* insert u stat_pon
			INSERT_STATUS(xtab_stat, pproizvod, xbroj, pstatus, DATE(), xopis)
			*
			* insert u unx_log
			IF xpuni_log
				puni_log(xbroj, , xid_log, xtablica)
			ENDIF
		ENDIF
	ENDSCAN
	pgc_bls(1, .T., xtext)
	*
	IF xexcel_out				&& izbaciti prikaz u Excel za batch proceduru (auto_statpon)
		PRIVATE xvar, xvartmp
		xvar = 'Update_statusa_ponuda_'+ALLTRIM(pproizvod)+'_'+DTOS(DATE())+'_'+SYS(2)
		xvartmp = unx_tmp+xvar
		SELECT tmp1
		COPY TO &xvartmp TYPE XL5
		pgc_out(xvar, , 'EXCEL')
	ENDIF
	*
	USE IN tmp1
ENDIF
*
SELECT (ssel)
RETURN xret
ENDFUNC	&& upd_status


*******************************************************************************
FUNCTION upd_statpon
PRIVATE ssel, xret, xkljuc, xpoz, xstat, xdana, xid_log
ssel = SELECT()
*
xret = .F.
IF ptip='PONUDE'
	upd_env('', 'UPD_STATPON')
	xkljuc = ALLTRIM(STRTRAN(PGCENV(ssifra),'\'))
	xid_log = ALLTRIM(STRTRAN(PGCENV('ID_LOG'),'\'))
	xpoz = AT(';',xkljuc)
	xdana = VAL(LEFT(xkljuc, xpoz-1))
	xstat = SUBSTR(xkljuc,xpoz+1)
	*
	IF EMPTY(xstat) OR xdana=0
		abc_message(pgc_dlang('UPOZORENJE')+CHR(13)+CHR(13)+;
			pgc_dlang('Neispravan kljuË')+' #'+ALLTRIM(UPPER(ssifra))+;
			' ili za ovaj proizvod nije predvieno storniranje nerealiziranih ponuda')
	ELSE
		xret = .T.
		abc_message(pgc_dlang('Broj storniranih nerealiziranih ponuda')+CHR(13)+CHR(13)+ALLTRIM(STR(upd_status('PON', ssifra, xstat, xdana, 'Storno nerealizirane ponude'))))
	ENDIF
ELSE
	abc_message(pgc_dlang('UPOZORENJE')+CHR(13)+CHR(13)+pgc_dlang('Funkciju moûete pozvati samo nad ponudama osigurateljnog proizvoda!'))
ENDIF
*
SELECT (ssel)
RETURN xret
ENDFUNC	&& upd_statpon


*******************************************************************************
FUNCTION auto_statpon
PARAMETERS pgrupa
PRIVATE ssel, xret, xkljuc, xpoz, xstat, xdana, xsifra, xrbr, xlast, xtext, xfile, xid_log, xgrupa_pro
ssel = SELECT()
*
upd_env('', 'UPD_STATPON')
xid_log = ALLTRIM(STRTRAN(PGCENV('ID_LOG'),'\'))

xgrupa_pro = ''
IF PCOUNT()=1 AND !EMPTY(pgrupa)
	xgrupa_pro = ALLTRIM(STRTRAN(PGCENV('UPD_GRUPA'+ALLTRIM(pgrupa)),'\'))
	IF EMPTY(xgrupa_pro)
		xgrupa_pro = '.'
	ENDIF
ENDIF

xfile = unx_tmp+'LOG_Storno_nerealiziranih_ponuda.txt'
xrbr = 0
*
SpremiLog(xfile, REPLICATE('*',55))
SpremiLog(xfile, 'STORNO NEREALIZIRANIH PONUDA - '+;
	IIF(PCOUNT()=1 AND !EMPTY(pgrupa), 'GRUPA'+ALLTRIM(pgrupa), 'SVE')+;
	' ('+ DTOC(DATE())+' u '+TIME()+')')
SpremiLog(xfile, REPLICATE('-',55))
*
ml_sql([SELECT sifra, tab_pon FROM skup_os ORDER BY osn_vrst, sifra INTO CURSOR tmpskup_os])
xlast = _TALLY
*
SCAN ALL
	*
	xrbr = xrbr+1
	xtext = 'Obrada: '+sifra
	pgc_bls(xrbr/xlast, .F., xtext)
	*
	xsifra = sifra
	xkljuc = ALLTRIM(STRTRAN(PGCENV(xsifra),'\'))
	*
	*!* IF !EMPTY(xkljuc)
	IF (EMPTY(xgrupa_pro) OR xsifra $ xgrupa_pro) AND !EMPTY(xkljuc)
		xpoz = AT(';',xkljuc)
		xdana = VAL(LEFT(xkljuc, xpoz-1))
		xstat = SUBSTR(xkljuc,xpoz+1)
		*
		IF !(EMPTY(xstat) OR xdana=0)

			xret = upd_status('PON', xsifra, xstat, xdana, 'Storno nerealizirane ponude', .F.)

			SpremiLog(xfile, xsifra+': '+ALLTRIM(STR(xret)))
			SpremiLog(xfile, REPLICATE('-',55))
		ENDIF
	ENDIF
ENDSCAN
pgc_bls(1, .T., xtext)
USE IN tmpskup_os
*
SpremiLog(xfile, 'ZAVRäETAK OBRADE ('+ DTOC(DATE())+' u '+TIME()+')')
SpremiLog(xfile, REPLICATE('*',55))
*
SELECT (ssel)
RETURN .T.
ENDFUNC	&& auto_statpon


*******************************************************************************
* Marko (09.01.2019) - storniranje nerealiziranih ponuda po grupama
*******************************************************************************
FUNCTION auto_stp
PARAMETERS pgrupa
IF PCOUNT()=1 AND !EMPTY(pgrupa)
	auto_statpon(pgrupa)
ELSE
	auto_statpon()
ENDIF
RETURN .T.
ENDFUNC	&& auto_stp


*******************************************************************************
* Marko (20.12.2017) - Promjena flaga "Faktur. sve rate" na poslovnom partneru
*******************************************************************************
FUNCTION fakt_sve_r
PRIVATE ssel, xok, xjmbg
ssel = SELECT()
*
xok = .F.
IF !('+FSVER+'$qovlasti)
	abc_message('UPOZORENJE'+CHR(13)+CHR(13)+'Nemate ovlast promjene tog podatka partnera'+CHR(13)+CHR(13)+'(Fakturiranje cijele police)')
	RETURN .F.
ELSE
	xjmbg = jmbg
	IF EMPTY(fakt_sve_r)
		IF abc_upit('N', 'éelite li omoguÊiti fakturiranje svih rata polica partnera?')
			IF ml_sql([UPDATE partneri SET fakt_sve_r=1 WHERE jmbg=?xjmbg])
				xok = .T.
			ENDIF
		ENDIF
	ELSE
		IF abc_upit('N', 'éelite li iskljuËiti fakturiranje svih rata polica partnera?')
			IF ml_sql([UPDATE partneri SET fakt_sve_r=0 WHERE jmbg=?xjmbg])
				xok = .T.
			ENDIF
		ENDIF
	ENDIF
ENDIF
*
SELECT (ssel)
RETURN xok
ENDFUNC	&& fakt_sve_r

*******************************************************************************
PROCEDURE pain007_xml
PRIVATE xputanja
	xputanja=ADDBS(ALLTRIM(unx_not)) + ADDBS(ALLTRIM(muser)) + "pain007.xml"
	qInsuranceObject.Pain007(broj, xputanja)
	abc_message('Kreirana je datoteka '+ xputanja)
ENDPROC	&& pain007_xml

*******************************************************************************
PROCEDURE pain008_xml
PRIVATE xputanja
	xputanja=ADDBS(ALLTRIM(unx_not)) + ADDBS(ALLTRIM(muser)) + "pain008.xml"
	qInsuranceObject.Pain008(broj, xputanja)
	abc_message('Kreirana je datoteka '+ xputanja)
ENDPROC	&& pain008_xml


*************************************************************************************************
* InCubis d.o.o.              sepa_odb		         				   (C) All rights reserved  *
* Author......: Zrinko PuljiÊ		                        				  Date: 01.03.2018  *
* Description.: Odbijene SEPA specifikacije													    *
*************************************************************************************************
PROCEDURE sepa_odb
PRIVATE ssel, xzaprimljena, xformat, xdana, xbr_spec, xds
ssel = SELECT()
*
xok = .F.
xzaprimljena = ''
*
datafstring = 'sepa_odb'
xformat = 'XML'
xdana = 15
xbr_spec = broj
ml_sql([SELECT serial FROM sepa_nal WHERE 1=2 INTO CURSOR ttemp_sepa READWRITE])
DO FORM sepa_odb
IF xok = .T.
	xds = loDotNetBridge.cursortodataset('ttemp_sepa')
	xxml=qReportsObject.ObavNenaplPrem(xbr_spec, xds, xformat,  xdana)
	IF !EMPTY(xxml)
		abc_message(xxml)
	ELSE
		abc_message(qInsuranceObject.Error)
	ENDIF
ENDIF
*
USE IN ttemp_sepa
SELECT (ssel)
RETURN
ENDPROC	&& sepa_odb


*************************************************************************************************
* InCubis d.o.o.              sepa_pd		         				   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ		                        				  Date: 15.03.2018  *
* Description.: Potpisivanje/Deaktivacija suglasnosti										    *
*************************************************************************************************
PROCEDURE sepa_pd
PARAMETERS ppotp
PRIVATE ssel, xserial, xpolica, xod, xdo, xdatum, xsredstvo, xsektor, xdat_potp
ssel = SELECT()
*
xserial = serial
xpolica = polica
xdatum = DATE()
xsredstvo = ''
xdat_potp = dat_potp
xsektor = ''
*
IF !EMPTY(xdat_potp ) AND ppotp
	abc_message('Suglasnost je veÊ potpisana.')
	SELECT (ssel)
	RETURN
ENDIF

IF EMPTY(xdat_potp ) AND !ppotp
	abc_message('Suglasnost mora biti potpisana da bi se deaktivirala.')
	SELECT (ssel)
	RETURN
ENDIF

IF !EMPTY(vrijedi_do) AND !ppotp
	abc_message('Suglasnost je veÊ deaktivirana.')
	SELECT (ssel)
	RETURN
ENDIF

ml_sql([SELECT p.poc_dat, p.ist_dat, v.poc_osig, v.ist_osig, p.sektor ] + ;
   [ FROM police_all p ] + ;
   [ LEFT JOIN visegod v ON p.broj = v.broj ] + ;
   [ WHERE p.broj = ?xpolica ] + ;
   [ INTO CURSOR tpol])
SELECT tpol
IF RECCOUNT()==0
	abc_message('Nema police za suglasnost.')
	USE IN tpol
	SELECT (ssel)
	RETURN
ENDIF

xsektor = sektor
IF (ppotp)
	xod = IIF(ISNULL(poc_osig), poc_dat, poc_osig)
ELSE
	xod = xdat_potp
ENDIF
xdo = IIF(ISNULL(poc_osig), ist_dat,IIF(EMPTY(ist_osig),GOMONTH(poc_osig,12*100),ist_osig) )
USE IN tpol

xdatum = pgc_retval( 'Unesite datum (' + DTOC(xod) + '-' + DTOC(xdo) + ')', xdatum, 'RETVAL >= xod and RETVAL < xdo ')

IF xdatum >= xod and xdatum < xdo
	xsredstvo = IIF(ppotp, 'SDD', QInsuranceObject.GetDefaultnoSredstvoPlacanja(xsektor))
	QInsuranceObject.Aktivacija_Deaktivacija_Suglasnosti(IIF(ppotp,'ACTIVATE','DEACTIVATE'), xdatum, xserial, xsredstvo )
ELSE
	abc_message('Unijeli ste neispravan datum.')
ENDIF
*
SELECT (ssel)
RETURN
ENDPROC && sepa_pd


*************************************************************************************************
* InCubis d.o.o.              sepa_iban		         				   (C) All rights reserved  *
* Author......: Marko Bekafigo		                        				  Date: 06.09.2019  *
* Description.: SEPA - promjena IBAN-a															*
*************************************************************************************************
PROCEDURE sepa_iban
PRIVATE ssel, xok, datafstring, xserial, xoib, xpolica, xid, xugo_naz, xugo_adr, xugo_mje, xiban, xswift, xuvjet, xswift
ssel = SELECT()
*
xok = .F.
xserial = serial 
xoib = oib
xpolica = polica
xid = id
xugo_naz = ugo_naz
xugo_adr = ugo_adr
xugo_mje = ugo_mje
xiban = iban
xswift = swift
*
datafstring = 'sepa_iban'
DO FORM sepa_iban NAME &datafstring
IF xok
	ml_sql([UPDATE suglasnost SET iban=?xiban, swift=?xswift WHERE serial=?xserial])
ENDIF
RETURN
ENDPROC	&& sepa_iban


*************************************************************************************************
* InCubis d.o.o.						winc_scheduler				   (C) All rights reserved  *
* Author......: Andreas CrnkoviÊ											  Date: 20.04.2018  *
* Description.: WinCubis task scheduler															*
*************************************************************************************************
PROCEDURE winc_scheduler
	PRIVATE xdate, xprocedura, xerror, xnext_run, xlast_run, xserial, xend_date, xset
	xdate=DATE()
	xend_date={..}
	xerror=.F.
	ml_sql("select * from unx_task where enabled=1 and next_run<=?xdate and start_date<=?xdate and (end_date>=?xdate or end_date=?qempty_date) into cursor cctaskovi")
	IF _tally>0
		SCAN ALL
			xprocedura=ALLTRIM(poziv)
			IF !EMPTY(params)
				xprocedura=xprocedura+"("+ALLTRIM(params)+")"
			ENDIF
			ON ERROR xerror=.T.
			write_log("unx_task_log.txt","PoËetak izvröavanja metode "+xprocedura)
			&xprocedura
			IF xerror
				xerror=.F.
				write_log("unx_task_log.txt","Pogreöka prilikom izvröavanja metode "+xprocedura+". Program: "+PROGRAM()+"; Linija: "+TRANSFORM(LINENO())+"; Poruka: "+MESSAGE())
			ELSE
				xserial=serial
				xlast_run=DATE()
				xend_date=IIF(EMPTY(end_date), qempty_date, end_date)
				xnext_run=qcubisobject.TaskNextRun(ALLTRIM(freq_type), ALLTRIM(freq_int), ALLTRIM(freq_rel), ALLTRIM(freq_rec), start_date, xend_date)
				xset="set "+IIF(xnext_run<>qempty_date,"next_run=?xnext_run, ","") + "last_run=?xlast_run"
				ml_sql("update unx_task " + xset + " where serial=?xserial")
				write_log("unx_task_log.txt","Kraj izvröavanja metode "+xprocedura)
			ENDIF
			ON ERROR pgc_error(LINENO())
		ENDSCAN
		USE IN cctaskovi
	ENDIF
ENDPROC

*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ												  Date: 11.05.2018  *
* Description.: Zaprimanje privole 																*
*************************************************************************************************
PROCEDURE privzapr
PARAMETERS pid, pdatum

IF VARTYPE(pdatum) != 'D'
	pdatum = DATE()
ENDIF

RETURN QInsuranceObject.GDPRZaprimi_privolu(pid, pdatum)

ENDPROC

*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ												  Date: 11.05.2018  *
* Description.: Potpis privole 																*
*************************************************************************************************
PROCEDURE privpotp
PARAMETERS pid, pdatum

IF VARTYPE(pdatum) != 'D'
	pdatum = DATE()
ENDIF

RETURN QInsuranceObject.GDPRPotpisi_privolu(pid, pdatum)

ENDPROC


*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ												  Date: 11.05.2018  *
* Description.: Opoziv privole 																	*
*************************************************************************************************
PROCEDURE privopoziv
PARAMETERS pid, pdatum

IF VARTYPE(pdatum) != 'D'
	pdatum = DATE()
ENDIF

RETURN QInsuranceObject.GDPROpoziv_privole(pid, pdatum)

ENDPROC


*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ												  Date: 11.05.2018  *
* Description.: Prigovor izravnom marketingu													*
*************************************************************************************************
PROCEDURE prigizvmarkt
PARAMETERS pjmbg
IF abc_upit('N', 'éelite li oznaËiti prigovor izravnom marketingu?')
	RETURN QInsuranceObject.Prigovor_izravmarkt(pjmbg)
ENDIF
RETURN .F.
ENDPROC


*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Marko Bekafigo												  Date: 04.09.2018  *
* Description.: Prigovor na segmentaciju													*
*************************************************************************************************
PROCEDURE prigov_seg
PARAMETERS pjmbg
PRIVATE ssel, xprigov_seg
ssel = SELECT()
*
IF PCOUNT()=0 OR EMPTY(pjmbg)
	abc_message(pgc_dlang('UPOZORENJE')+CHR(13)+CHR(13)+pgc_dlang('Neispravan ulazni parametar funkcije'))
	RETURN .F.
ENDIF
*
xprigov_seg = prigov_seg
IF abc_upit('N', 'éelite li '+IIF(xprigov_seg=0, 'ukljuËiti', 'iskljuËiti')+' prigovor na segmentaciju?')
	RETURN QInsuranceObject.Prigovor_segmentacija(pjmbg, IIF(xprigov_seg=0, 1, 0))
ENDIF
*
SELECT (ssel)
RETURN .F.
ENDPROC


*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Marko Bekafigo												  Date: 04.09.2018  *
* Description.: Prigovor na ispitivanje zadovoljstva											*
*************************************************************************************************
PROCEDURE prigov_zad
PARAMETERS pjmbg
PRIVATE ssel, xprigov_zad
ssel = SELECT()
*
IF PCOUNT()=0 OR EMPTY(pjmbg)
	abc_message(pgc_dlang('UPOZORENJE')+CHR(13)+CHR(13)+pgc_dlang('Neispravan ulazni parametar funkcije'))
	RETURN .F.
ENDIF
*
xprigov_zad = prigov_zad
IF abc_upit('N', 'éelite li '+IIF(xprigov_zad=0, 'ukljuËiti', 'iskljuËiti')+' prigovor na ispitivanje zadovoljstva?')
	RETURN QInsuranceObject.Prigovor_ispitzad(pjmbg, IIF(xprigov_zad=0, 1, 0))
ENDIF
*
SELECT (ssel)
RETURN .F.
ENDPROC


*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ												  Date: 17.05.2018  *
* Description.: Provjera pravila anonimizacije													*
*************************************************************************************************
PROCEDURE ANOPROVZAH
QCubisObject.Provjeri_zahtjeveano()
ENDPROC


*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ												  Date: 21.05.2018  *
* Description.: Unos teksta obrade																*
*************************************************************************************************
PROCEDURE ANOZAHOBR
	PRIVATE gnapomena, xid, gok, XBOT
	xid = id
	gok = .f.
	XBOT = 0
	gnapomena = ''
	DO FORM napomena
	IF gok
		qcubisobject.ZapisiComentObr((xid),(gnapomena))
	ENDIF
ENDPROC


*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ												  Date: 21.05.2018  *
* Description.: Promjena statusa										 						*
*************************************************************************************************
PROCEDURE ANOZAHSTA
	PRIVATE xstatus, xid, xok
	xid = id
	xok= .f.
	xstatus= ''
	xstatus=PGC_RETVAL('Unesite novi status', (xstatus), "EXT(.F.,RETVAL,'RETVAL','ANO_STAT','SIFRA')")
	IF !EMPTY(ALLTRIM(xstatus))
		qcubisobject.PromijeniStatus((xid),(xstatus))
		RETURN .T.
	ELSE
		RETURN .F.
	ENDIF
ENDPROC


*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ												  Date: 21.05.2018  *
* Description.: Anonimizacija											 						*
*************************************************************************************************
PROCEDURE ANONIMIZA
	qCubisObject.AnonimizirajZahtjeve()
ENDPROC


*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Toni Gregov / Nikola MilkoviÊ								  Date: 30.05.2018  *
* Description.: IMPORT POMAGALA									 								*
*************************************************************************************************
PROCEDURE DZOPOMIM
PRIVATE xfile, xdir, xdat_now, xdat_empty
	xfile=GETFILE()
	xdat_now = DATE()
	xdat_empty = DATE(1901,01,01)
	IF FILE(xfile)
		abc_message('Pokrenuti Êe se procedura koja Êe potrajati, nemojte gasiti WinCubis. Javiti Êe vam se poruka kada je import zavröio.')
		qInsuranceObject.importPomagala(xfile, xdat_now, xdat_empty)
		abc_message('IMPORT uspjeöan.')
	ELSE
		abc_message('Niste odabrali datoteku.')
	ENDIF

ENDPROC	&& dzopomim


*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Maja JahiÊ													  Date: 13.06.2018  *
* Description.: Generiranje id suglasnosti i iznosa zaduûenja za TN								*
*************************************************************************************************
PROCEDURE TN_SUGLAS
PRIVATE xfile, xret
	xfile=GETFILE()
	IF FILE(xfile)
		xret = qInsuranceObject.Ugo_TnSuglasnost(xfile)
		abc_message(xret)
	ELSE
		abc_message('Niste odabrali datoteku.')
	ENDIF

ENDPROC	&& tn_suglas


*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ								  				  Date: 21.06.2018  *
* Description.: IMPORT WFORCE BLAGAJNE									 						*
*************************************************************************************************
PROCEDURE WFORCEBL
PARAMETERS PFILE

	qInsuranceObject.WFORCEBL((PFILE))

ENDPROC	&& WFORCEBL

*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Toni Gregov (dll) /Nikola MilkoviÊ							  Date: 21.06.2018  *
* Description.: IMPORT WFORCE AO									 							*
*************************************************************************************************
PROCEDURE WFORCEAO
PARAMETERS PFILE

	qInsuranceObject.WFORCE_AO((PFILE))

ENDPROC	&& WFORCEAO

*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Draûenka LadiöiÊ (dll) /Nikola MilkoviÊ						  Date: 21.06.2018  *
* Description.: IMPORT WFORCE AK									 							*
*************************************************************************************************
PROCEDURE WFORCEAK
PARAMETERS PFILE

	qInsuranceObject.WFORCE_AK((PFILE))

ENDPROC	&& WFORCEAK


*************************************************************************************************
* InCubis d.o.o.              wforce		         				   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ		                        				  Date: 21.06.2018  *
* Description.: Odbijene SEPa specifikacije													    *
*************************************************************************************************
PROCEDURE wforce
PARAMETERS pbezforme, pbrisi_starefile
PRIVATE ssel, xfile, xformat, xdirAO, xdirAK, xdirBL, xcd_old
ssel=SELECT()
xok=.F.

xcd_old = SYS(2003)
*
IF !pbezforme
	datafstring = 'SEPA_ODB'
	xfile = ''
	DO FORM wforce
	IF xok=.T.
		DO CASE
		CASE xformat= 'AO'
			WFORCEAO(xfile)
		CASE xformat= 'AK'
			WFORCEAK(xfile)
		CASE xformat= 'BL'
			WFORCEBL(xfile)
		OTHERWISE
			abc_message('Neispravan format datoteke.')
		ENDCASE
	ENDIF
ELSE
	xdirAO = PGCENV('WFORCEAO')
	xdirAK = PGCENV('WFORCEAK')
	xdirBL = PGCENV('WFORCEBL')
	xdirOBR = PGCENV('WFORCEOBR')
	ON ERROR pda()

	DIMENSION xfilesAO(1)
	DIMENSION xfilesAK(1)
	DIMENSION xfilesBL(1)
	CD &xdirAO
	IF (ADIR(xfilesAO)>0)
		FOR X= 1 TO ALEN(xfilesAO,1)
			XFILE = xfilesAO(x,1)
			XFILECOPY = xdirOBR + DTOS(DATETIME())+ 'AO_'+ SYS(3)+ XFILE
			COPY FILE &XFILE TO &XFILECOPY
			WFORCEAO(xfile)
			IF pbrisi_starefile
				DELETE FILE &XFILE
			ENDIF
		ENDFOR
	ENDIF
	CD &xdirAK
	IF (ADIR(xfilesAK)>0)
		FOR X= 1 TO ALEN(xfilesAK,1)
			XFILE = xfilesAK(x,1)
			XFILECOPY = xdirOBR + DTOS(DATETIME())+ 'AK_'+ SYS(3)+ XFILE
			COPY FILE &XFILE TO &XFILECOPY
			WFORCEAK(xfile)
			IF pbrisi_starefile
				DELETE FILE &XFILE
			ENDIF
		ENDFOR
	ENDIF
	CD &xdirBL
	IF (ADIR(xfilesBL)>0)
		FOR X= 1 TO ALEN(xfilesBL,1)
			XFILE = xfilesBL(x,1)
			XFILECOPY = xdirOBR + DTOS(DATETIME())+ 'BL_'+ SYS(3)+ XFILE
			COPY FILE &XFILE TO &XFILECOPY
			WFORCEBL(xfile)
			IF pbrisi_starefile
				DELETE FILE &XFILE
			ENDIF
		ENDFOR
	ENDIF
	ON ERROR pgc_error(LINENO())
ENDIF
CD &xcd_old
RETURN
ENDPROC	&& wforce


*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Maja JahiÊ													  Date: 26.06.2018  *
* Description.: Generiranje xml obavijesti za partnere radi prelaska s TN na SEPU				*
*************************************************************************************************
PROCEDURE TN_OBAV
PRIVATE xret
xret = qInsuranceObject.Ugo_TnObavijest()
abc_message(xret)
ENDPROC	&& tn_obav

*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ												  Date: 09.07.2018  *
* Description.: Generiranje xml uplatnica														*
* Dorada: dodan UPP za WIENER
*************************************************************************************************
PROCEDURE dzxmlupl
PRIVATE xret,xok, xod, xdo, xproizvod, datafstring,xpla_id
STORE '' TO  xproizvod
STORE DATE() TO xod, xdo
xok = .f.
datafstring = 'xml_uplatnice'
xpla_id = ''
IF fqmat_broj('KVIG')
	xpla_id = 'UPP'
ENDIF

DO FORM xml_uplatnice
IF xok=.T.
	ABC_MESSAGE('Zbog koliËine podataka moguÊe je da ova procedura potraje, prikazati Êe vam se poruka kada procedura zavröi.')
	qreportsobject.xmlIspisiViseUplatnicaDZO(xproizvod, xod,xdo, xpla_id,.F.)
	ABC_MESSAGE('Xml datoteka s podacima se nalazi u vaöem notes folderu.')
ENDIF
ENDPROC	&& tn_obav


*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ												  Date: 17.07.2018  *
* Description.: Aûuriranje datuma isteka														*
*************************************************************************************************
PROCEDURE dzocjnewdat
PARAMETERS PID_USTANOVE,PDATUM
PRIVATE ssel xustanova, xdatum
ssel = SELECT()
IF !abc_upit('N', 'éelite li aûurirati datum do CJENIKA za ustanovu?')
	RETURN .F.
ENDIF
IF VARTYPE(PID_USTANOVE) == 'N'
	xustanova = PID_USTANOVE
ELSE
	xustanova = id
ENDIF
xdatum = DATE()
ML_SQL([SELECT max(vrij_do) as do FROM dzoustugo WHERE id_ustanove = ?xustanova into cursor tdo])
IF !ISNULL(do) AND xdatum < do
	xdatum = do
ENDIF
USE IN tdo
IF VARTYPE(PDATUM) == 'D'
	xdatum = PDATUM
ELSE
	xdatum = pgc_retval( 'Unesite novi datum isteka:', xdatum, 'RETVAL >DATE() ')
ENDIF

IF xdatum >DATE()
	QInsuranceObject.dzocjenik_azur_datisteka((xustanova), (xdatum ))
ENDIF

ABC_MESSAGE('Uspjeöno je aûuriran datum isteka.')

SELECT(ssel)

RETURN .t.

ENDPROC


*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ												  Date: 17.07.2018  *
* Description.: Aûuriranje datuma isteka														*
*************************************************************************************************
PROCEDURE ustnewugovor
PRIVATE ssel, xustanova_newugo, xdatum
ssel = SELECT()

PRIVATE nulfunc, izjfunc, getfunc, setfunc
nulfunc = 'nuldzoustugo'
izjfunc = 'izjdzoustugo'
getfunc = 'getdzoustugo'
setfunc = 'setdzoustugo'

xustanova_newugo = id
ML_SQl([select * from dzoustugo WHERE 1=2 into cursor ttempustugo readwrite ])
selt = SELECT()

xvar=PGC_VAR()
PRIVATE &xvar
STORE '' TO &xvar
DIMENSION  ATR(1), ZAG(1)

ATR(1)= ''
ZAG(1)= ''
load_dd('PRIJAVA', 'ATR', 'DZOUSTUGO')
exp_atr('DZOUSTUGO')

nogrid((selt), 'I', , , 'Unos ugovora za ustanovu','DZOUSTUGO')

ATR(1)= ''
ZAG(1)= ''
load_dd('PRIJAVA', 'ATR', 'DZO_USTANOVE')
exp_atr('DZO_USTANOVE')
SELECT(ssel)
RETURN .t.
ENDPROC


***************************************************************
* Naziv:	Tisak uplatnice (akcija nad POLI02 ili PONU02)
* Poziv:	DO TIS_UPLH ili TIS_UPLH()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	31.07.2018.
***************************************************************
PROCEDURE tis_uplh
PRIVATE ssel, xbroj, xproizvod, xpolice, xxml
* ---
ssel = SELECT()
xbroj = broj
xproizvod = proizvod

IF stab_pol <> 'POLI02' AND stab_pol <> 'PONU02'
	abc_message('Funkcija je predviena samo za zdravstveno osiguranje.')
	SELECT (SSel)
	RETURN
ENDIF
WAIT WINDOW NOWAIT 'Priprema izvjeötaja u tijeku...'
*
xpolice = IIF(ptip='POLICE', .T., .F.)
xxml=qReportsObject.IspisUplatnicaRata(xbroj, xproizvod, '', xpolice)
*
DO xml_out WITH xxml, IIF(fqmat_broj('UNIQA'),'tisuplUN','tisupltr')
SELECT (SSel)
RETURN
ENDPROC && tis_uplh


***************************************************************
* Naziv:	Provjera izmjene police
* Poziv:	provj_pol()
* Opis:		()
* Autor:	Nikola MilkoviÊ
* Datum:	16.08.2018.
***************************************************************
PROCEDURE provj_prom_pol
PARAMETERS pbroj, pproizvod
PRIVATE xok
xok = .f.
xok=qInsuranceObject.provj_prom_pol(pbroj, pproizvod)
RETURN xok
ENDPROC && provj_prom_pol



***************************************************************
* Naziv:	Provjera izmjene police
* Poziv:	provj_pol()
* Opis:		()
* Autor:	Nikola MilkoviÊ
* Datum:	16.08.2018.
***************************************************************
PROCEDURE write_prom_pol
PARAMETERS pbroj, pproizvod
PRIVATE xok
xok = .f.
xok=qInsuranceObject.write_prom_pol(pbroj, pproizvod)
RETURN xok
ENDPROC && write_prom_pol

***************************************************************
* Naziv:	Provjera izmjene police
* Poziv:	provj_pol()
* Opis:		()
* Autor:	Nikola MilkoviÊ
* Datum:	16.08.2018.
***************************************************************
PROCEDURE delete_prom_pol
PARAMETERS pbroj, pproizvod
PRIVATE xok
xok = .f.
xok=qInsuranceObject.delete_prom_pol(pbroj, pproizvod)
RETURN xok
ENDPROC && delete_prom_pol

***************************************************************
* Naziv:	Tisak IPID-a
* Poziv:	TisakIPID()
* Opis:		Akcija nad policom za tisak IPID-a preko Wienerovog Web servisa
* Autor:	Andreas CrnkoviÊ
* Datum:	11.09.2018.
***************************************************************
FUNCTION TisakIPID
PRIVATE ssel, xtiskan_org, xidnamobj, xtablica, xbrdok, xmessage, xstream, xproizvod, xbroj, xid_grup, xid_cjen, xid_sura
ssel = SELECT()
*
xproizvod = proizvod
xbroj = broj
xid_grup = id_grup
xid_cjen = id_cjen
xid_sura = id_sura
*
xtablica = IIF(ptip='POLICE', 'imoprpol', 'imoprpon')
xpolica = (ptip='POLICE')
xidnamobj = ret_xt1([id_namobj], xtablica, [polica=?xbroj], '')
STORE '' TO xbrdok, xmessage
*
xstream = qInsuranceObject.IpidDoc(xbrdok, xmessage, !xpolica, xbroj, xproizvod, xid_grup, xid_cjen, qInsuranceObject.VratiStatistikePoPolici(xbroj, !xpolica), xidnamobj, xid_sura)
*
IF !QCubisObject.IsEmptyStream(xstream)
	xpath = STRTRAN(unx_tmp,'\','\\') + SYS(3)+ '.pdf'
	QCubisObject.StreamToFile(xstream, xpath)
	xtiskan_org = .F.
	IF FILE(xpath)
		pdf_screen(STRTRAN(xpath,'\\','\'), .T.) 	&& prikazi ga na ekran	&& Marko (21.05.2018) (parametar pdelete)
		xtiskan_org = .T.
	ENDIF
	IF !xtiskan_org
		abc_message(pgc_dlang('Doölo je do greöke prilikom tiska!')+CHR(13)+CHR(13)+;
			pgc_dlang('Kontaktirajte administratora.'))
		SELECT (ssel)
		RETURN .F.
	ENDIF
ENDIF
IF !EMPTY(STRTRAN(STRTRAN(xmessage,'Nije pronaen niti jedan dokument',''),';','')) AND !INLIST(proizvod,'04','05','06','07','11','12','BK','PK','SO','YK','KY')
	abc_message(pgc_dlang('Doölo je do greöke u web servisu!')+CHR(13)+CHR(13)+;
		pgc_dlang('Kontaktirajte administratora.')+CHR(13)+CHR(13)+;
		xmessage)
	SELECT (ssel)
	RETURN .F.
ENDIF
*
SELECT (ssel)
RETURN .T.
ENDFUNC && TisakIPID


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* Incubis d.o.o.                      prom_svrh_izv (C) All rights reserved 					*
* Author......: Zrinko PuljiÊ                                            Date: 13.10.2018		*
* Description.: Promjena statusa tuûbe
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
PROCEDURE prom_svrh_izv
PARAMETERS psveizv
PRIVATE xsvrha, ssel, xok , xrecno, gid_trans, giznos, gnerasp, gserial
ssel = SELECT()
*
IF ABC_UPIT('Y', 'Jeste li sigurni da ûelite promijeniti svrhu na izvodima? ')
	xsvrha = ''
	xok = .F.
	DO WHILE !xok
		xsvrha = PGC_RETVAL('Nova SVRHA:',xsvrha , "Ext(.T.,RetVal,'RetVal','SVRHE','SVRHA')")
		*PGC_RETVAL('Nova prodajna struktura:',xprodstru, "Ext(.T.,RetVal,'RetVal','PRODSTRU','ID_PS')")
		xok = EXT(.F.,xsvrha ,'xsvrha ','svrhe','svrha')
	ENDDO
	SELECT(ssel)
	IF !psveizv
		xok = (gid_trans == 'NP' AND giznos == gnerasp)
		IF xok
			ML_SQL([UPDATE st_izvod SET svrha=?xsvrha WHERE serial=?gserial])
		ELSE
			ABC_MESSAGE('Ovu stavku izvoda nije moguÊe aûurirati!')
		ENDIF
	ELSE
		xrecno = -1
		SELECT(ssel)
		SCAN ALL
			xrecno = RECNO()
			pgc_bls(RECNO()/RECCOUNT(),.F.,'Obrada podataka...')
			SELECT(ssel)
			*
			gid_trans = id_trans
			giznos = iznos
			gnerasp = nerasp
			gserial = serial
			*
			xok = (gid_trans == 'NP' AND giznos == gnerasp)
			IF xok
				ML_SQL([UPDATE st_izvod SET svrha=?xsvrha WHERE serial=?gserial])
			ENDIF
			IF !xok
				LOOP
			ENDIF
			SELECT(ssel)
			IF XRECNO <> -1
				GO XRECNO
			ENDIF
		ENDSCAN
	ENDIF
	pgc_bls(1,.T.)
ENDIF
SELECT(ssel)
ENDPROC

***************************************************************
* Naziv:	Poravnjane ukupnog iznosa stavki autorizacije sa tocnim ukupnim iznosom autorizacije
* Poziv:	ispriznstrac()
* Opis:		()
* Autor:	Boris
* Datum:	24.10.2018.
***************************************************************
PROCEDURE ispriznstrac
PRIVATE xok
xok = .f.
xok=qInsuranceObject.PoravnajUkupniIznos(serial)
RETURN xok
ENDPROC && ispriznstrac



***************************************************************
* Naziv:	Inicijalno kreiranje prava suradnika
* Poziv:	InsertPravaSur()
* Opis:		()
* Autor:	Nikola
* Datum:	12.11.2018.
***************************************************************
PROCEDURE InsertPravaSur
PRIVATE xok, ssel

ssel = SELECT()
xok = .f.

*briöi prava za one kojima Êeö ih rekreirati

ml_sql([delete from sur_rights where id_sura in (select id_sura  from suradnci where raz_pol != '8' and raz_pol != '' and login !='' and dat_blok = ?qempty_date )])

ml_sql([select id_sura  from suradnci where raz_pol != '8' and raz_pol != '' and login !='' and dat_blok = ?qempty_date into cursor tsur ])

SELECT tsur
GO TOP
SCAN ALL
	pgc_bls(RECNO()/RECCOUNT(),.F.,'Obrada podataka...')
	xok=qInsuranceObject.kreirajPravaSura((id_sura))
	SELECT tsur
ENDSCAN
USE IN tsur

pgc_bls(1,.T.)

SELECT(ssel )
RETURN xok
ENDPROC &&


***************************************************************
* Naziv:	Aûuriranje prava sura
* Poziv:	AzurirajPravaSur()
* Opis:		()
* Autor:	Nikola
* Datum:	12.11.2018.
***************************************************************
FUNCTION AzurirajPravaSur
PARAMETERS pid_sura
PRIVATE xok, ssel
ssel = SELECT()
*
* (1) Kreiraj prava
xok = qInsuranceObject.kreirajPravaSura((pid_sura))
*
* (2) Dodaj ga nadreenima
qInsuranceObject.InsertNadredenihSurrights((pid_sura))
*
SELECT (ssel)
RETURN xok
ENDFUNC && AzurirajPravaSur


***************************************************************
* Naziv:	Raspodjela uplata s datotekama
* Poziv:	RaspodjelaUPL()
* Opis:		()
* Autor:	Boris i Nikola
* Datum:	20.11.2018.
***************************************************************
PROCEDURE  RaspodjelaUPL
PRIVATE ssel, xok, xfile, xsel, xalias,xprebaci,XUSER, yserial
ssel = SELECT()
xok = .T.
xfile = ''
xfile1= ''
xfile = GETFILE('*','Odaberite datoteku s postojeÊim uplatama')
IF EMPTY(ALLTRIM(xfile))
	abc_message('Niste odabrali datoteku s postojeÊim uplatama!')
	SELECT(ssel)
	RETURN
ENDIF
xfile1 = GETFILE('*','Odaberite datoteku s novim uplatama')
IF EMPTY(ALLTRIM(xfile1))
	abc_message('Niste odabrali datoteku s novim uplatama!')
	SELECT(ssel)
	RETURN
ENDIF

xok = QInsuranceObject.GetRaspodijeljeneUplate(xfile, xfile1)

IF !xok
	abc_message('Obrada uplata je neuspjeöna!')
	SELECT(ssel)
	RETURN
ENDIF
ENDPROC


***************************************************************
* Naziv:	NoÊno zatvaranje uplata
* Poziv:	zatvpr_noc()
* Opis:		()
* Autor:	Nikola
* Datum:	07.12.2018.
***************************************************************
PROCEDURE  zatvpr_noc
	QINSURANCEOBJECT.nocno_zatvnaplate()
ENDPROC


***************************************************************
* Naziv:	Slanje polica iz osiguranja u CVH
* Poziv:	Send_cvh()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	18.12.2018.
* Izmjene:	09.07.2019 za Wiener izbaËena strana vozila, izvozne tablice i prijevozniËka odgovornost
***************************************************************
PROCEDURE Send_cvh
PRIVATE ssel, xdatum_od, xdatum_do, xds, xerror, xdsrez

ssel = SELECT()
STORE DATE() TO xdatum_od, xdatum_do
xerror = ''
PGC_RET2VAL('Slanje polica u HUO','PoËetni datum','Krajnji datum','xdatum_od','xdatum_do')
ml_sql([SELECT a.broj AS broj_police, a.br_sasije AS broj_sasije, b.pdv_br AS oib_osiguranika, a.reg_ozn AS reg_oznaka, ] +;
	[ a.poc_dat AS vrijedi_od, a.poc_sat AS vrijeme_od, a.ist_dat AS vrijedi_do, a.ist_sat AS vrijeme_do ] +;
	[ FROM poli10 a, partneri b ] +;
	[ WHERE a.osi_id = b.jmbg ] + IIF(xfqmat_kva, [ AND a.proizvod <> 'SV' AND a.proizvod <> 'IZ' AND a.proizvod <> 'PO' ], []) +;
	[ AND a.datum BETWEEN ?xdatum_od AND ?xdatum_do AND a.sektor <> '9' AND a.id_status <> 'S9' ] +;
	[ INTO CURSOR ccpolice])

IF _tally>0
	xds = loDotNetBridge.cursortodataset('ccpolice')
	IF VARTYPE(xds)='O'
		xdsrez = qInsuranceObject.cvhteh_prihvat_polica(xds, xerror)
		IF VARTYPE(xdsrez)<>'X' AND EMPTY(xerror)
			xfile = 'CVH_prihvat_polica' + '_' + TTOC(DATETIME(),1) + '.xlsx'
			qCubisObject.WriteXlsx_ds(xdsrez, unx_not+ADDBS(TRIM(muser)) + xfile)
			abc_message('U vaöem notes folderu nalazi se ' + xfile + ' datoteka s rezultatima.')
			TRY
				oexcel=CREATEOBJECT("Excel.Application")
				oexcel.VISIBLE=.T.
				ON ERROR abc_error(0,'Excel datoteka nije kreirana...')
				oexcel.workbooks.OPEN(unx_not+ADDBS(TRIM(muser)) + xfile)
			CATCH
				*
			ENDTRY
		ELSE
			abc_message(xerror)
		ENDIF
	ELSE
		abc_message('UPOZORENJE'+CHR(13)+CHR(13)+'Greöka u pripremi podataka')
	ENDIF
	USE IN ccpolice
ENDIF
SELECT(ssel)
ENDPROC && Send_cvh


***************************************************************
* Naziv:	Dohvat statusa za viöe polica osiguranja
* Poziv:	Dohv_n_stat()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	24.12.2018.
***************************************************************
PROCEDURE Dohv_n_stat
PRIVATE ssel, xbroj_pol, xprijenos_id, xerror, xdsrez

ssel = SELECT()
STORE 0 TO xbroj_pol, xprijenos_id
xerror = ''
xbroj_pol = pgc_retval('Dohvat statusa za viöe polica. Broj polica:',xbroj_pol)
IF xbroj_pol>0
	xdsrez = qInsuranceObject.cvhteh_dohvati_status_za_vise_polica(xbroj_pol, xprijenos_id, xerror)
	IF VARTYPE(xdsrez)<>'X' AND EMPTY(xerror)
		xfile = 'CVH_status_' + ALLTRIM(STR(xbroj_pol)) + '_polica_ID_' + ALLTRIM(STR(xprijenos_id)) + '.xlsx'
		qCubisObject.WriteXlsx_ds(xdsrez, unx_not+ADDBS(TRIM(muser)) + xfile)
		MESSAGEBOX('U vaöem notes folderu nalazi se ' + xfile + ' datoteka s rezultatima.')
		TRY
			oexcel=CREATEOBJECT("Excel.Application")
			oexcel.VISIBLE=.T.
			ON ERROR abc_error(0,'Excel datoteka nije kreirana...')
			oexcel.workbooks.OPEN(unx_not+ADDBS(TRIM(muser)) + xfile)
		CATCH
			*
		ENDTRY
	ELSE
		abc_message(xerror)
	ENDIF
ENDIF
SELECT(ssel)
ENDPROC && Dohv_n_stat


***************************************************************
* Naziv:	Ponovljeni dohvat statusa za viöe polica osiguranja
* Poziv:	Dohv_n_stat2()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	24.12.2018.
***************************************************************
PROCEDURE Dohv_n_stat2
PRIVATE ssel, xbroj_pol, xprijenos_id, xerror, xdsrez

ssel = SELECT()
STORE 0 TO xbroj_pol, xprijenos_id
xerror = ''
PGC_RET2VAL('Ponovljeni dohvat statusa za viöe polica','Broj polica','ID prijenosa','xbroj_pol','xprijenos_id')
IF xbroj_pol>0 AND xprijenos_id>0
	xdsrez = qInsuranceObject.cvhteh_dohvati_status_za_vise_polica_ponovljeni(xbroj_pol, xprijenos_id, xerror)
	IF VARTYPE(xdsrez)<>'X' AND EMPTY(xerror)
		xfile = 'CVH_ponovljeni_status_' + ALLTRIM(STR(xbroj_pol)) + '_polica_ID_' + ALLTRIM(STR(xprijenos_id)) + '.xlsx'
		qCubisObject.WriteXlsx_ds(xdsrez, unx_not+ADDBS(TRIM(muser)) + xfile)
		MESSAGEBOX('U vaöem notes folderu nalazi se ' + xfile + ' datoteka s rezultatima.')
		TRY
			oexcel=CREATEOBJECT("Excel.Application")
			oexcel.VISIBLE=.T.
			ON ERROR abc_error(0,'Excel datoteka nije kreirana...')
			oexcel.workbooks.OPEN(unx_not+ADDBS(TRIM(muser)) + xfile)
		CATCH
			*
		ENDTRY
	ELSE
		abc_message(xerror)
	ENDIF
ENDIF
SELECT(ssel)
ENDPROC && Dohv_n_stat2


***************************************************************
* Naziv:	Dohvat liste identifikatora prijenosa koji su veÊi od poslanog
* Poziv:	Dohv_n_id()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	24.12.2018.
***************************************************************
PROCEDURE Dohv_n_id
PRIVATE ssel, xprijenos_id, xerror, xdsrez

ssel = SELECT()
xprijenos_id = 0
xerror = ''
xprijenos_id = pgc_retval('Dohvat identifikatora prijenosa. ID veÊi od:',xprijenos_id)
xdsrez = qInsuranceObject.cvhteh_dohvati_status_za_vise_polica_iskoristeni(xprijenos_id, xerror)
IF VARTYPE(xdsrez)<>'X' AND EMPTY(xerror)
	xfile = 'CVH_identifikatori_prijenosa_veÊi od_' + ALLTRIM(STR(xprijenos_id)) + '.xlsx'
	qCubisObject.WriteXlsx_ds(xdsrez, unx_not+ADDBS(TRIM(muser)) + xfile)
	MESSAGEBOX('U vaöem notes folderu nalazi se ' + xfile + ' datoteka s rezultatima.')
	TRY
		oexcel=CREATEOBJECT("Excel.Application")
		oexcel.VISIBLE=.T.
		ON ERROR abc_error(0,'Excel datoteka nije kreirana...')
		oexcel.workbooks.OPEN(unx_not+ADDBS(TRIM(muser)) + xfile)
	CATCH
		*
	ENDTRY
ELSE
	abc_message(xerror)
ENDIF
SELECT(ssel)
ENDPROC && Dohv_n_id

***************************************************************
* Naziv:	parsedatum
* Poziv:	parsedatum(<string>)
* Opis :	Parsira string formata dd.mm.yyyy ili d.m.yyyy i vraÊa datum
* Autor:	Goran äirola, Marko Bekafigo
* Datum:	21.01.2019
***************************************************************
FUNCTION parsedatum
PARAMETERS pdatum, pdefa
PRIVATE xret
IF GETWORDCOUNT(pdatum, '.') = 3 AND ;
	TYPE(GETWORDNUM(pdatum, 3, '.')) == 'N' AND ;
	TYPE(GETWORDNUM(pdatum, 2, '.')) == 'N' AND ;
	TYPE(GETWORDNUM(pdatum, 1, '.')) == 'N'
	xret = DATE(VAL(GETWORDNUM(pdatum, 3, '.')), VAL(GETWORDNUM(pdatum, 2, '.')), VAL(GETWORDNUM(pdatum, 1, '.')))
ELSE
	xret = IIF(PCOUNT()<2, qempty_date, pdefa)
ENDIF
RETURN xret
ENDFUNC && parsedatum

***************************************************************
* Naziv:	Definiranje izuzetka algoritma zatvaranja naplate nad partnerima
* Poziv:	DO ISKLJ_ZNAPL ili ISKLJ_ZNAPL()
* Opis:		(akcija nad pojedinim i cijelom listom partnera)
* Autor:	Maja JahiÊ
* Datum:	24.01.2019.
* Izmjene:  06.03.2019 - omoguÊeno da jedan partner moûe imati viöe aktivnih razliËitih tipova izuzetaka
***************************************************************
PROCEDURE isklj_znapl
PARAMETERS psto
PRIVATE ssel, xrecno, xjmbg, xalgoritam, xser, xok, xpzatvexcel
* ---
ssel = SELECT()
xrecno = -1
xalgoritam = '1'
xok = .F.
xpzatvexcel = ''
datafstring='ISKLJ_ZNAPL'
DO FORM isklj_znapl NAME &datafstring
IF xok
	IF !gsmart
		xjmbg = jmbg
		xser = get_ser()
		xpzatvexcel = RET_XT1('algoritam', 'pzatvexcl', [jmbg=']+ALLTRIM(xjmbg)+[' and algoritam=']+ALLTRIM(xalgoritam)+[' and akt=1], SPACE(13))
		IF xpzatvexcel = SPACE(13)
			IF ml_sql([INSERT INTO pzatvexcl (serial, jmbg, algoritam, napomena, akt) ]+ ;
					[VALUES (?xser, ?xjmbg, ?xalgoritam, '', 1)])
				puni_log(xjmbg, , '01', 'pzatvexcl')
			ELSE
				abc_error(0, 'Zapis promjena nije uspio - molimo obavijestite informatiËku sluûbu!')
			EXIT
			ENDIF
		ELSE
			abc_message('Za partnera veÊ postoji aktivan slog za algoritam ' + ALLTRIM(xpzatvexcel))
		ENDIF
	ELSE
		SELECT (ssel)
		GO TOP
		SCAN ALL
			xrecno = RECNO()
			xjmbg = jmbg
			xser = get_ser()
			xpzatvexcel = RET_XT1('algoritam', 'pzatvexcl', [jmbg=']+ALLTRIM(xjmbg)+[' and algoritam=']+ALLTRIM(xalgoritam)+[' and akt=1], SPACE(13))
			IF xpzatvexcel = SPACE(13)
				IF ml_sql([INSERT INTO pzatvexcl (serial, jmbg, algoritam, napomena, akt) ]+ ;
						[VALUES (?xser, ?xjmbg, ?xalgoritam, '', 1)])
					puni_log(xjmbg, , '01', 'pzatvexcl')
					SELECT (ssel)
					IF xrecno <> -1
						GO xrecno
					ENDIF
				ELSE
					abc_error(0, 'Zapis promjena nije uspio - molimo obavijestite informatiËku sluûbu!')
				EXIT
				ENDIF
			ELSE
				abc_message('Za partnera veÊ postoji aktivan slog za algoritam ' + ALLTRIM(xpzatvexcel))
			ENDIF
		ENDSCAN
	ENDIF
ENDIF
*
SELECT (SSel)
RETURN
ENDPROC && isklj_znapl

***************************************************************
* Naziv:	Nova noÊna procedura zatvaranja naplate
* Poziv:	Zatvnoc
* Opis:		Nova noÊna procedura zatvaranja naplate u kojoj uvjete definira korisnik
* Autor:	Nikola MilkoviÊ
* Datum:	08.02.2019.
***************************************************************
PROCEDURE Zatvnoc
	QINSURANCEOBJECT.Zatvnoc()
ENDPROC


***************************************************************
* Naziv:	Akcija aktivacije/deaktivacije
* Poziv:	aktnaplnoc(id, .t.) ili aktnaplnoc(id, .F.)
* Opis:		Nova noÊna procedura zatvaranja naplate u kojoj uvjete definira korisnik
* Autor:	Nikola MilkoviÊ
* Datum:	08.02.2019.
***************************************************************
PROCEDURE aktnaplnoc
	PARAMETERS pid, paktiviraj
	qSifarniciObject.AktivirajDeaktivirajNaplNoc(pid, paktiviraj)
	RETURN .t.
ENDPROC


***************************************************************
* Naziv:	Tisak dopisa za obnovu i predugovorne dokumentacije za proizvod DI
* Poziv:	DOPOBN_IPID('S') ili DOPOBN_IPID('D') sve ili samo predugovorna dokumentacija
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	06.03.2019
***************************************************************
PROCEDURE dopobn_ipid
	PARAMETERS psto
	PRIVATE ssel, xrec, xproizvod, xpolica, xrezdat
	ssel = SELECT()
	xproizvod = proizvod
	*
	IF xproizvod <> 'DI'
		abc_message('UPOZORENJE'+CHR(13)+CHR(13)+'Funkcija dostupna samo za osigurateljni proizvod DI')
		RETURN .F.
	ENDIF
	*
	xrec = RECNO()
	IF ptip = 'POLICE'
		xpolica = .T.
	ELSE
		xpolica = .F.
	ENDIF
	*
	SELECT * FROM ALIAS() INTO CURSOR tmp_pon_di ORDER BY broj
	IF _tally > 0
		CursorToXML("tmp_pon_di","xtmp_pon_di",1,16)
		USE IN tmp_pon_di
		xrezdat = qReportsObject.TisakDopisaObnoveIPredugovorneDokum(xtmp_pon_di, xpolica, psto)
		IF LEN(ALLTRIM(xrezdat)) > 0
			MESSAGEBOX('PDF datoteka je spremljena: ' + ALLTRIM(xrezdat), 64)
		ELSE
			IF !EMPTY(qReportsObject.Error)
				abc_error(0, qReportsObject.Error)
			ELSE
				DO MSG_NEMA
			ENDIF
		ENDIF
	ELSE
		abc_message('UPOZORENJE'+CHR(13)+CHR(13)+'Nisu odabrane police za kreiranje pdf dokumenta.')
	ENDIF
	*
	SELECT (ssel)
	GOTO xrec
	RETURN .T.
ENDPROC && dopobn_ipid


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* Naziv:	Izmjena broja grupne police za proizvod DI (akcija nad objektom)
* Poziv:	PROM_GRPOL()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	14.05.2019
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
FUNCTION prom_grpol
	PRIVATE ssel, xy, xid_grpol, xret
	xret=.F.
	xy=RECNO()
	ssel=SELECT()
	IZJ_NUL('IZJ', 'G', '*')
	IF gproizvod <> 'DI'
		abc_message(pgc_dlang('Akcija je predviena samo za proizvod DI')+'.')
	ELSE
		IF !EMPTY(gdat_ver)
			xid_grpol=gid_grpol
			xid_grpol=ALLTRIM(pgc_retval('Unesite novi broj grupne police: '+gbroj, (xid_grpol)))
			IF !EMPTY(xid_grpol)
				IF ret_xt1('oznaka', 'grup_pol', 'oznaka=?xid_grpol', ' ') <> ' '
					ml_sql([UPDATE ]+stab_pol+[ SET id_grpol=?xid_grpol WHERE broj=?gbroj])
					xret=.T.
					abc_message(pgc_dlang('Promijenjen je broj grupne police')+'.')
				ELSE
					abc_message(pgc_dlang('Uneseni broj grupne police ne postoji u öifarniku')+'.')
				ENDIF
			ENDIF
		ELSE
			abc_message(pgc_dlang('Akcija nije predviena za netarifirane police')+'. '+pgc_dlang('Izmjenite direktno u formi police')+'.')
		ENDIF
	ENDIF
	SELECT (ssel)
	GOTO xy
	RETURN xret
ENDFUNC && prom_grpol


***************************************************************
* Naziv:	Kreiranje SEPA suglasnosti po polici
* Poziv:	kreirajsugl()
* Opis:		()
* Autor:	Nikola MilkoviÊ
* Datum:	22.05.2019
***************************************************************
FUNCTION kreirajsugl
PRIVATE xiban, xbroj, xproizvod

STORE '' TO xiban, xbroj, xproizvod
xiban=ALLTRIM(PGC_RETVAL('Unesite IBAN:',xiban))
IF qInsuranceObject.provjeriIBAN((xiban))
	xbroj = broj
	xproizvod = proizvod
	IF qInsuranceObject.KreirajSuglasnost(xbroj, xiban, xproizvod,'')
		*
	ELSE
		abc_message(qInsuranceObject.Error)
	ENDIF
ELSE
	abc_message('Uneöeni IBAN nije ispravan ponovite unos.')
ENDIF
ENDFUNC


***************************************************************
* Naziv:	Automatski export AXA za HOK
* Poziv:	AXA_EXP()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	03.06.2019
***************************************************************
PROCEDURE axa_expa
	axa_exp(1)
RETURN
ENDPROC && axa_expa


***************************************************************
* Naziv:	Export AXA za HOK
* Poziv:	AXA_EXP()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	03.06.2019
***************************************************************
PROCEDURE axa_exp
PARAMETERS pauto
	PRIVATE xod, xdo, xret, xizlazna_datoteka
	STORE DATE() TO xod, xdo
	*
	IF PCOUNT() = 0
		xret = pgc_ret2val('Period za AXA report', 'Od datuma', 'Do datuma', 'xod', 'xdo')
	ENDIF
	xizlazna_datoteka = qInsuranceObject.ExportAXA(xod, xdo, UNX_TPL + 'AXA_report.xlsx')
	IF PCOUNT() = 0
		IF !EMPTY(xizlazna_datoteka)
			abc_message('Rezultat je spremljen u notes korisnika ' + xizlazna_datoteka)
		ELSE
			IF !EMPTY(qReportsObject.Error)
				IF qReportsObject.Error <> '0 '
					abc_error(0, qReportsObject.Error)
				ELSE
					abc_message(SUBSTR(qReportsObject.Error, 3))
				ENDIF
			ELSE
				DO msg_nema
			ENDIF
		ENDIF
	ENDIF
RETURN
ENDPROC && axa_exp

*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ												  Date: 09.07.2018  *
* Description.: Generiranje xml uplatnica WIENER UPX											*
*************************************************************************************************
PROCEDURE dzxmluplw
PRIVATE xret,xok, xod, xdo, xproizvod, datafstring,xpla_id
STORE '' TO  xproizvod
STORE DATE() TO xod, xdo
xok = .f.
datafstring = 'xml_uplatnice'
xpla_id = ''
IF fqmat_broj('KVIG')
	xpla_id = 'UPX'
ENDIF
DO FORM xml_uplatnice
IF xok=.T.
	ABC_MESSAGE('Zbog koliËine podataka moguÊe je da ova procedura potraje, prikazati Êe vam se poruka kada procedura zavröi.')
	qreportsobject.xmlIspisiViseUplatnicaDZO(xproizvod, xod,xdo,xpla_id,.T.)
	ABC_MESSAGE('Xml datoteka s podacima se nalazi u vaöem notes folderu.')
ENDIF
ENDPROC	&& tn_obav

*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ	- Mario dll									  Date: 31.07.2019  *
* Description.: Import utuûenja					 												*
*************************************************************************************************
PROCEDURE ImportUtuzenja
PRIVATE ssel, xok, xfile, xsel
ssel = SELECT()
xok = .T.
xfile = ''
xfile = GETFILE()
IF EMPTY(ALLTRIM(xfile))
	abc_message('Niste odabrali datoteku za import!')
	SELECT(ssel)
	RETURN
ENDIF

IF !qinsuranceobject.ImportTuzbi(xfile)
	abc_message(qinsuranceobject.error)
ENDIF

SELECT(ssel)
RETURN

ENDPROC


*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ	- Boris dll									  Date: 31.07.2019  *
* Description.: Generiranje xml - triglav zlikovac												*
*************************************************************************************************
PROCEDURE XMLOtplSDD
PRIVATE xfile, ssel
ssel = SELECT()

xdateOd = DATE(2019,02,01)
xdateDo = DATE(2019,06,04)
IF qinsuranceobject.GenerirajXMListuOtplatnihPlanova(xdateOd ,xdateDo )
	abc_message('U vaöem notes folderu nalazi se xml datoteka za tiskaru.')
ELSE 
	abc_message(qinsuranceobject.Error)
ENDIF 
*file =
ENDPROC
*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ	- Tomislav dll								  Date: 14.08.2019  *
* Description.: Aktivacija/deaktivacija 												*
*************************************************************************************************
PROCEDURE akttuxcl
PARAMETERS psifra
qinsuranceobject.UtuzenjaAktFlg(psifra)
RETURN .T.
ENDPROC


*************************************************************************************************
* InCubis d.o.o.													   (C) All rights reserved  *
* Author......: Nikola MilkoviÊ												Date: 21.08.2019  	*
* Description.: stornodidd 																		*
*************************************************************************************************
PROCEDURE stornodidd
PARAMETERS pbroj, pproizvod, pprovjereno
PRIVATE ssel, xok, pgcvars, xstornokar, xserial, xbr_fakt, xziro, xiznos_30
ssel = SELECT()
*
xok = .F.
xstornokar = .F.
xserial = 0
xbr_fakt = ''
xiznos_30 = 0
xziro = STRTRAN(PGCENV('ZIRO_DIDD'),'\')
*
*
ml_sql([SELECT * FROM ]+stab_pol+[ WHERE broj=?pbroj INTO CURSOR tpoli])
pgcvars = pgc_var()
PRIVATE &pgcvars
STORE '' TO &pgcvars
IZJ_NUL('IZJ', 'G', '*')
*
IF xfqmat_kva AND (!pprovjereno OR qinsuranceobject.PostojiStetPoPoliciDIDD(gbroj, gproizvod))

	IF !pprovjereno OR abc_upit('N', 'Ovo je polica '+gproizvod+' koja je napravljena poslije 15.03.2019. éelite li storno NE fakturiranih rata? (D/N)')

		PRIVATE giznosd, gopis, xproduz, xmonolitna, gtip_prov, gdat_dosp, gdat_zad

		gopis = 'Storno DIDD'
		gtip_prov = '1'
		xproduz = .F.
		xmonolitna  = .F.
		gdat_zad = DATE()
		gdat_dosp = DATE()
		giznosd = qinsuranceobject.VratiIznosStornaWienerDIDD_OP1(gbroj)

		DO FORM stornodidd

		IF xok

			ml_sql([SELECT * FROM ]+stab_pol+[ WHERE broj=?pbroj INTO CURSOR tpoli])
			gid_status = id_status
			
			
			xiznos_30 = qinsuranceobject.VratiTrecinuPremije(gbroj)
			
			
			qinsuranceobject.storno_pol(gbroj, gproizvod, gid_status, gdat_zad, giznosd, gist_dat)
			
			IF xstornokar 
				qinsuranceobject.storno_kar_poPol(gbroj, 'SO', gist_dat)
			ENDIF 
			
			gpla_id = 'STD'
			
			giznosd = giznosd  + xiznos_30 
			
			DO getdd IN policed
			
			ml_sql([update ] + stab_pol + [ set pla_id = ?gpla_id  WHERE broj=?pbroj ])
			IF !EMPTY(ALLTRIM(xziro))
			
				ml_sql(	[ SELECT serial ] + ;
						[ FROM st_prem ] + ;
						[ WHERE polica = ?gbroj and pla_id = 'STD' and iznos - iznosp > 0 ] + ;
						[ INTO CURSOR tserfakt ])
						
				IF (RECCOUNT()=1)
					xserial = serial
					
					Faktura_DIDD(xserial, gsektor, xziro, (sosn_vrst), (stab_pol), xbr_fakt)
					
					IF VARTYPE(xbr_fakt)=='C' AND !EMPTY(ALLTRIM(xbr_fakt))
						ABC_MESSAGE('Kreirana je faktura broj:' + xbr_fakt)
					ENDIF 
					
				ENDIF 
				USE IN tserfakt 
			ENDIF 
			 
			SELECT (ssel)
			DO gettis IN POLICES5 WITH (stab_pol) ,(pbroj)
			
			xok = .T.
		ENDIF
	ENDIF
ELSE
	IF !EMPTY(qInsuranceObject.Error)
		abc_message(ALLTRIM(qInsuranceObject.Error))
	ENDIF
ENDIF
USE IN tpoli
*
SELECT (ssel)
RETURN xok


***************************************************************
* Naziv:	Reaktivacija DZO kartice (akcija nad DZO_KART)
* Poziv:	DO REAK_DZKAR ili REAK_DZKAR()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	05.09.2019.
***************************************************************
PROCEDURE reak_dzkar
PRIVATE ssel, xserial, xstatus, xbr_pol, xrez
* ---
ssel = SELECT()
xrez=''
xserial = serial
xstatus = status
xbr_pol = polica
xrez = qInsuranceObject.ReaktivacijaKartice(xserial, xstatus, xbr_pol)
abc_message(xrez)
SELECT (SSel)
RETURN
ENDPROC && reak_dzkar


***************************************************************
* Naziv:	Promjena prodajne strukture suradnika po svim tablicama u sustavu
* Poziv:	PROM_SURPS()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	14.10.2019
***************************************************************
FUNCTION prom_surps
PRIVATE ssel, xid_sura, datafstring, xniz_ps, xps_old, xps_new
ssel = SELECT()
*
DIMENSION xniz_ps[1,2]
STORE '' TO xps_old, xps_new
xid_sura = id_sura

ml_sql([SELECT distinct(a.prodstru) AS prodstru, b.opis ] + ;
	[ FROM police_all a, prodstru b ] + ;
	[ WHERE a.prodstru = b.id_ps AND a.id_sura = ?xid_sura ] + ;
	[ ORDER BY prodstru ] + ;
	[ INTO ARRAY xniz_ps])
IF _TALLY>0
	datafstring = 'PROM_SURPS'
	DO FORM prom_surps NAME &datafstring
	IF !EMPTY(ALLTRIM(xps_old)) AND !EMPTY(ALLTRIM(xps_new))
		abc_wait(pgc_dlang('Promjena PS'))
		qInsuranceObject.Promijeni_PS(ALLTRIM(xid_sura), ALLTRIM(xps_old), ALLTRIM(xps_new))
	ENDIF
ENDIF
*
SELECT (ssel)
RETURN
ENDFUNC && prom_surps

***************************************************************
* Naziv:	Promjena prodajne strukture suradnika po policama i ponudama
* Poziv:	PROM_SURPS2()
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	02.04.2020
***************************************************************
FUNCTION prom_surps2
PRIVATE ssel, xid_sura, datafstring, xniz_ps, xps_old, xps_new, xod, xdo
ssel = SELECT()
*
DIMENSION xniz_ps[1,2]
STORE '' TO xps_old, xps_new
xod = GOMONTH(DATE(), -1)
xdo = DATE()
xid_sura = id_sura

ml_sql([SELECT distinct(a.prodstru) AS prodstru, b.opis ] +;
	[ FROM police_all a, prodstru b ] +;
	[ WHERE a.prodstru = b.id_ps AND a.id_sura = ?xid_sura ] +;
	[ UNION ALL ] +;
	[ SELECT distinct(a.prodstru) AS prodstru, b.opis ] +;
	[ FROM ponude_all a, prodstru b ] +;
	[ WHERE a.prodstru = b.id_ps AND a.id_sura = ?xid_sura ] +;
	[ INTO CURSOR ccprodstru])

ml_sql([SELECT prodstru, opis ] +;
	[ FROM ccprodstru ] +;
	[ GROUP BY prodstru, opis ] +;
	[ INTO ARRAY xniz_ps])

IF _TALLY>0
	datafstring = 'PROM_SURPS2'
	DO FORM prom_surps2 NAME &datafstring
	IF !EMPTY(ALLTRIM(xps_old)) AND !EMPTY(ALLTRIM(xps_new)) AND !EMPTY(xod) AND !EMPTY(xdo)
		abc_wait(pgc_dlang('Promjena PS'))
		qInsuranceObject.Promijeni_PS_pol_pon(ALLTRIM(xid_sura), ALLTRIM(xps_old), ALLTRIM(xps_new), xod, xdo)
	ENDIF
ENDIF
*
SELECT (ssel)
RETURN
ENDFUNC && prom_surps2


***************************************************************
* Naziv:	ret_tblpol
* Poziv:	PROM_SURPS()
* Opis:		VraÊa naziv tablice polica prema broju police u prijavi öteta
*			(defaultno vraÊa tablicu polica iz knjige öteta - ptabpol)
* Autor:	Marko Bekafigo
* Datum:	07.11.2019
***************************************************************
FUNCTION ret_tblpol
PARAMETERS ppol, ptabpol
PRIVATE ssel, xret
ssel = SELECT()
*
IF EMPTY(ppol)
	RETURN ptabpol
ENDIF
*
xret = ptabpol
ml_sql([SELECT s.tab_pol ]+;
	[ FROM police_osn p ]+;
		[ INNER JOIN skup_os s ]+;
		[ ON p.proizvod=s.sifra ]+;
	[ WHERE p.broj=?ppol ]+;
	[ INTO CURSOR tmptblpol])
IF _TALLY>0 AND !EMPTY(tab_pol)
	xret = tab_pol
ENDIF
USE IN tmptblpol
*
SELECT (ssel)
RETURN xret
ENDFUNC && ret_tblpol


***************************************************************
* Naziv:	sap_nocna
* Poziv:	sap_nocna()
* Opis:		NoÊna procedura za premijsko SAP
* Autor:	Nikola MilkoviÊ 
* Datum:	26.11.2019
***************************************************************
FUNCTION sap_nocna
	Qinsuranceobject.NocnaProceduraPremijsko()
ENDFUNC	&& sap_nocna

***************************************************************
* Naziv:	Prebacivanje na nove obrasce za tisak polica AO po korisniku
* Poziv:	NoviRep()
* Opis:		()
* Autor:	Olivera
* Datum:	07.04.2020
***************************************************************
FUNCTION NoviRep
PRIVATE SSEL, xporuka 
SSEL=SELECT()
xporuka=''

xporuka=qreportsobject.prelazak_na_novi_rep((muser))

abc_message(xporuka)

SELECT(SSEL)
RETURN .T.

***************************************************************
* Naziv:	Unos izuzetaka za utuûenja po OIB-u partnera
* Poziv:	TUZEXCL_OIB() ili DO TUZEXCL_OIB
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	21.04.2020
***************************************************************
PROCEDURE tuzexcl_oib
PRIVATE ssel, xsifra, xpdv_br, xproizvod, xpolica, xproizvex, seltmp, xcount, xy, xok
STORE '' TO xpdv_br, xproizvod, xpolica, xproizvex
STORE '*' TO xid_agen, xid_sura
xsifra = 'Novi'
xok = .F.
xy=RECNO()
ssel=SELECT()
*
ml_sql([CREATE CURSOR tmpskup (izabran L(1), jmbg C(13), naziv C(60))])
seltmp=SELECT()
*
*poziv forme
datafstring = 'TUZEXCL_OIB'
DO FORM tuzexcl_oib NAME &datafstring
*
IF xok
	SELECT (seltmp)	
	xcount = 0
	* inicijalno su svi partneri izabrani
	SCAN ALL FOR izabran=.T.
		xjmbg = jmbg
		IF !get_counter('TUZBEXCL', xcount)
			abc_message('Neispravna dodjela broja zapisa!')
			SELECT (ssel)
			RETURN
		ELSE
			xsifra = PADL(ALLTRIM(STR(xcount)), 7, '0')
		ENDIF
		ml_sql([INSERT INTO tuzbexcl (sifra, id_part, id_agen, id_sura, polica, ] +;
			[ napomena, aktivan, datum_prom, korisnik, proizvod, pla_id, proizvex) ] +;
			[ VALUES (?xsifra, ?xjmbg, ?xid_agen, ?xid_sura, ?xpolica, ?xnapomena, 1, ] +;
			[ ?DATE(), ?muser, ?xproizvod, ?xpla_id, ?xproizvex)])
	ENDSCAN
ENDIF
SELECT (ssel)
GOTO xy
RETURN
ENDPROC && tuzexcl_oib

***************************************************************
* Naziv:	Punjenje stavki kandidatima po OIB-u
* Poziv:	KAND_TUZEXCL() ili DO KAND_TUZEXCL
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	22.04.2020
***************************************************************
PROCEDURE kand_tuzexcl
PRIVATE ssel
ssel=SELECT()
ml_sql([SELECT p.jmbg, p.naziv ] +;
	[ FROM partneri p ]+;			
	[ WHERE p.pdv_br=?xpdv_br AND p.status<>'9' ]+;
	[ AND p.jmbg NOT IN ] +;
	[ (SELECT t.id_part FROM tuzbexcl t WHERE t.id_part=p.jmbg AND aktivan=1) ] +;
	[ ORDER BY p.jmbg ]+;
	[ INTO CURSOR t100])
IF _TALLY>0
	SELECT tmpskup
	APPEND FROM DBF('t100')
ENDIF
USE IN t100
SELECT tmpskup
SCAN ALL
	REPLACE izabran WITH .T.
ENDSCAN
GO TOP
SELECT (ssel)
RETURN
ENDPROC && kand_tuzexcl


***************************************************************
* Naziv:	Aktivacija izuzetaka za utuûenja po OIB-u partnera
* Poziv:	TUZAKTOIB() ili DO TUZAKTOIB
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	30.04.2020
***************************************************************
PROCEDURE tuzaktoib
PRIVATE ssel, xoib, xok
ssel=SELECT()
xoib = pgc_retval('OIB', '', "ext(.T., retval, 'retval', 'partneri', 'pdv_br')")
IF !EMPTY(xoib)
	xok = qInsuranceObject.UtuzenjaPromjAkt(xoib, .T.)
	IF xok
		abc_message('Partneri su aktivirani')
	ENDIF
ENDIF
SELECT (ssel)
RETURN
ENDPROC


***************************************************************
* Naziv:	Deaktivacija izuzetaka za utuûenja po OIB-u partnera
* Poziv:	TUZNEAKTOIB() ili DO TUZNEAKTOIB
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	30.04.2020
***************************************************************
PROCEDURE tuzneaktoib
PRIVATE ssel, xoib, xok
ssel=SELECT()
xoib = pgc_retval('OIB', '', "ext(.T., retval, 'retval', 'partneri', 'pdv_br')")
IF !EMPTY(xoib)
	xok = qInsuranceObject.UtuzenjaPromjAkt(xoib, .F.)
	IF xok
		abc_message('Partneri su deaktivirani')
	ENDIF
ENDIF
SELECT (ssel)
RETURN
ENDPROC


***************************************************************
* Naziv:	Promjena nadleûnog JB na viöe poötanskih brojeva
* Poziv:	PROMJ_PTT_JB() ili DO PROMJ_PTT_JB
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	30.04.2020
***************************************************************
PROCEDURE promj_ptt_jb
PRIVATE ssel, xpartner_old, xpartner_new, xok
xok = .F.
ssel=SELECT()
datafstring = 'PROMJ_PTT_JB'
DO FORM promj_ptt_jb NAME &datafstring
IF xok AND !EMPTY(xpartner_old) AND !EMPTY(xpartner_new)
	xok = qInsuranceObject.UtuzenjaPromjPttJB(xpartner_old, xpartner_new)
	IF xok
		abc_message('Javni biljeûnik je izmijenjen')
	ENDIF
ENDIF
SELECT (ssel)
RETURN
ENDPROC


***************************************************************
* Naziv:	Funkcija otkljuËava izvode odabranog korisnika koji su ostali zakljuËani
* Poziv:	OTKLJ_ZATVNAPL()
* Autor:	Maja JahiÊ
* Datum:	06.05.2020
***************************************************************
PROCEDURE otklj_zatvnapl
PRIVATE ssel, XOnOff, xuser_zatvnapl
ssel = SELECT()
*
xuser_zatvnapl = ALLTRIM(STRTRAN(PGCENV('USER_ZATVNAPL'),'\'))
ml_sql([SELECT z.korisnik, u.abcime ] +; 
	[ FROM zatvnapl z, unx_user u ] +;
	[ WHERE z.korisnik=u.abcuser AND u.uni00=?unx_dir ] +;
	[ GROUP BY z.korisnik, u.abcime ] +;
	[ ORDER BY z.korisnik ] +;
	[ INTO CURSOR tmpzatvnapl])
XOnOff=SET('CURSOR')
SET CURSOR ON
xuser_zatvnapl = pgc_retval('Korisnik:', xuser_zatvnapl, 'Ext(.F.,RetVal,"RetVal","tmpzatvnapl.ext_cursor","korisnik")')
SET CURSOR &XOnOff
IF ret_xt1('COUNT(*)', 'zatvnapl', 'korisnik=?xuser_zatvnapl', 0) > 0
	IF abc_upit('Y','éelite li otkljuËati izvode korisnika: ' + ALLTRIM(xuser_zatvnapl) + '?')
		IF qInsuranceObject.OtkljucajIzvodePoKorisniku(xuser_zatvnapl)
			abc_message('Izvodi su otkljuËani.')
		ENDIF
	ENDIF
ELSE
	abc_message('Korisnik nema zakljuËanih izvoda.')
ENDIF
*
USE IN tmpzatvnapl
SELECT (ssel)
RETURN
ENDFUNC


***************************************************************
* Naziv:	Aûuriranje pol/pon/fakt naknadnim unosom ugovora o javnoj nabavi
* Poziv:	JNAB_AZURDOK() ili DO JNAB_AZURDOK
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	14.05.2020
***************************************************************
PROCEDURE jnab_azurdok
PRIVATE ssel, xpdv_br, xid, seltmp, seltmr, xy, xok
xpdv_br = oib
xid = id
xok = .F.
xy=RECNO()
ssel=SELECT()
*
ml_sql([CREATE CURSOR temp_ponude (izabran L(1), broj C(13), poc_dat D, ist_dat D, proizvod C(2))])
seltmp=SELECT()

ml_sql([CREATE CURSOR temp_police (izabran L(1), broj C(13), poc_dat D, ist_dat D, proizvod C(2))])
seltmr=SELECT()
*
*poziv forme
datafstring = 'JNAB_AZURDOK'
DO FORM jnab_azurdok NAME &datafstring
*
IF xok
	*ponude
	SELECT * FROM temp_ponude WHERE izabran INTO CURSOR azur_pon
	IF RECCOUNT() > 0
		xdspon = loDotNetBridge.cursortodataset('azur_pon')
		qInsuranceObject.PolPonAzuriranjeJavnaNabava(xdspon, xid, .F.)
	ENDIF
	*police
	SELECT * FROM temp_police WHERE izabran INTO CURSOR azur_pol
	IF RECCOUNT() > 0
		xdspol = loDotNetBridge.cursortodataset('azur_pol')
		qInsuranceObject.PolPonAzuriranjeJavnaNabava(xdspol, xid, .T.)
	ENDIF
	IF LEFT(qInsuranceObject.Error, 1) = '0'
		abc_message(SUBSTR(qInsuranceObject.Error, 3))
	ELSE
		abc_error(0, qInsuranceObject.Error)
	ENDIF
ENDIF
SELECT (ssel)
GOTO xy
RETURN
ENDPROC && jnab_azurdok

***************************************************************
* Naziv:	Punjenje stavki ponuda i polica (ugovor o javnoj nabavi)
* Poziv:	KAND_AZUR_JNAB() ili DO KAND_AZUR_JNAB
* Opis:		()
* Autor:	Maja JahiÊ
* Datum:	14.05.2020.
***************************************************************
PROCEDURE kand_azur_jnab
PRIVATE ssel, xmlpol, xmlpon, xcount_pol, xcount_pon
ssel=SELECT()
*police
xmlpol = qInsuranceObject.PolPonKandidatiAzuriranjeJavnaNabava(xid, .T.)
IF !EMPTY(xmlpol)
	XMLTOCURSOR(xmlpol,'temp1_police')
	ml_sql([SELECT .T. AS izabran, broj, poc_dat, ist_dat, proizvod ] +;
		[ FROM temp1_police INTO CURSOR temp2_police])
	ZAP IN temp_police
	SELECT temp_police
	APPEND FROM DBF ('temp2_police')
	USE IN temp1_police
	USE IN temp2_police
ENDIF
SELECT temp_police
xcount_pol = RECCOUNT()
GO TOP
*ponude
xmlpon = qInsuranceObject.PolPonKandidatiAzuriranjeJavnaNabava(xid, .F.)
IF !EMPTY(xmlpon)
	XMLTOCURSOR(xmlpon,'temp1_ponude')
	ml_sql([SELECT .T. AS izabran, broj, poc_dat, ist_dat, proizvod ] +;
		[ FROM temp1_ponude INTO CURSOR temp2_ponude])
	ZAP IN temp_ponude
	SELECT temp_ponude
	APPEND FROM DBF ('temp2_ponude')
	USE IN temp1_ponude
	USE IN temp2_ponude
ENDIF
SELECT temp_ponude
xcount_pon = RECCOUNT()
GO TOP
IF xcount_pol = 0 AND xcount_pon = 0
	abc_message('Nema podataka')
ENDIF
SELECT (ssel)
RETURN
ENDPROC && kand_azur_jnab


FUNCTION brisi_skad
PRIVATE ssel, rr, xporuka
ssel = SELECT()
rr = RECNO()
xporuka = ''
gid_skad = id_skad
IF !('+TAR+'$qovlasti) 
	abc_message('Nemate ovlast !')
	SELECT (ssel)
	RETURN 
ENDIF
gpriprema = priprema
IF gpriprema=1
	abc_message('Priprema je proöla u redu , nema potrebe za ovom akcijom.')
	SELECT (ssel)
	RETURN 
ENDIF 
IF !abc_upit('Y','Jeste li sigurno da ûelite brisanje skadencara '+gid_skad+' ?')
	SELECT (ssel)
	RETURN 
ENDIF 
IF !abc_upit('Y','Akcija se koristi ako je doölo do pucanja pripreme skadencara '+gid_skad+'. Jeste li sigurno da ûelite brisanje skadencara ?')
	SELECT (ssel)
	RETURN 
ENDIF
IF !abc_upit('Y', 'Svi podaci skadencara '+gid_skad+' nad kojim se nalazite biti Êe obrisani. Jeste li sigurno da ûelite nastaviti sa brisanjem skadencara ?')
	SELECT (ssel)
	RETURN 
ENDIF

xporuka = qInsuranceObject.BrisiSkadencZbogGreske(gid_skad)
abc_message(xporuka)
DELETE FROM ALIAS(ssel) WHERE id_skad=gid_skad

SELECT (ssel)
goto rr
RETURN 


***************************************************************
* Naziv:	check_hok_digital
* Opis : 	prikaz da li je partner korisnik HOK Digitala
* Autor:	Marko Bekafigo
* Datum:	27.04.2021
***************************************************************
FUNCTION check_hok_digital
PARAMETERS xid, pind, plabel
PRIVATE xret
xret = plabel
IF !EMPTY(xid) AND xfqmat_hok

	* Za isporuku

	DO CASE
		CASE pind = '0'		&& äifra (default)
			xret = IIF(!EMPTY(qinsuranceobject.PortalUser(xid)), 'äIFRA (HOK Digital)', 'äifra')
		CASE pind = '1UGO'	&& Ugovaratelj (1)
			xret = IIF(!EMPTY(qinsuranceobject.PortalUser(xid)), 'Ugovaratelj(HD)', 'Ugovaratelj:')
		CASE pind = '1OSI'	&& Osiguranik (1)
			xret = IIF(!EMPTY(qinsuranceobject.PortalUser(xid)), 'Osiguranik (HD)', 'Osiguranik:')
		CASE pind = '1STE'	&& ätetnik (1)
			xret = IIF(!EMPTY(qinsuranceobject.PortalUser(xid)), 'ätetnik (HD)', 'Naziv ötetnika')
		CASE pind = '1OST'	&& OöteÊeni (1)
			xret = IIF(!EMPTY(qinsuranceobject.PortalUser(xid)), 'OöteÊeni (HD)', 'ID oöteÊenog')
	ENDCASE
	
	* Za testiranje
	
	*!*	DO CASE
	*!*		CASE pind = '0'		&& äifra (default)
	*!*			xret = IIF(!EMPTY(xid), 'äifra (HD)', 'äifra')
	*!*		CASE pind = '1UGO'	&& Ugovaratelj (1)
	*!*			xret = IIF(!EMPTY(xid), 'Ugovaratelj (HD)', 'Ugovaratelj:')
	*!*		CASE pind = '1OSI'	&& Osiguranik (1)
	*!*			xret = IIF(!EMPTY(xid), 'Osiguranik (HD)', 'Osiguranik:')
	*!*		CASE pind = '1STE'	&& ätetnik (1)
	*!*			xret = IIF(!EMPTY(xid), 'ätetnik (HD)', 'Naziv ötetnika')
	*!*		CASE pind = '1OST'	&& OöteÊeni (1)
	*!*			xret = IIF(!EMPTY(xid), 'OöteÊeni (HD)', 'ID oöteÊenog')
	*!*	ENDCASE

ENDIF
RETURN xret