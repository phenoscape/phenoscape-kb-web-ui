package org.phenoscape.kb.ui

import org.phenoscape.kb.ui.Model.Classification
import org.phenoscape.kb.ui.Model.IRI
import org.phenoscape.kb.ui.Model.Taxon
import org.phenoscape.kb.ui.Model.Term
import org.phenoscape.kb.ui.Vocab._

import outwatch.Sink
import outwatch.dom._
import outwatch.dom.Attributes.style
import outwatch.dom.Attributes.title
import outwatch.dom.VNode
import rxscalajs.Observable

object Views {

  def termName(iri: IRI): VNode = span(title := iri.id, child <-- KBAPI.termLabel(iri).map(_.label))

  def taxonName(taxon: Taxon): VNode = {
    val isGenusOrSpecies = taxon.rank.exists(rank => GenusOrSpecies(rank.iri.id))
    var classes = List("taxon-name")
    if (taxon.extinct) classes = "extinct" :: classes
    val genusSpecies = if (isGenusOrSpecies) "genus-species" else ""
    span(cls := classes.mkString(" "), title := taxon.iri.id, span(cls := genusSpecies, taxon.label))
  }

  def classification(data: Classification, termRenderer: Term => VNode): VNode = {
    val superClasses = Observable.of(data.subClassOf.map(term => li(termRenderer(term))))
    val equivalents = Observable.of(data.equivalentTo.map(term => span(" = ", termRenderer(term))))
    val subClasses = Observable.of(data.superClassOf.map(term => li(termRenderer(term))))
    div(
      cls := "classification-level",
      ul(cls := "list-unstyled", children <-- superClasses),
      div(
        cls := "classification-level",
        p(data.label, span(children <-- equivalents)),
        div(cls := "classification-level", ul(cls := "list-unstyled", children <-- subClasses))))
  }

  def formatSynonyms(syns: List[(IRI, String)]): VNode =
    if (syns.isEmpty) i("None")
    else {
      val synNodes = syns.sortBy(_._2.toLowerCase).map { case (relation, value) => span(value, " ", span(cls := "synonym-type", s"(${Vocab.synonymTypes(relation)})")) }
      span(Util.interpolate(span(", "), synNodes): _*)
    }

  def termInfoView(iri: IRI): VNode = {
    val term = KBAPI.termInfo(iri)
    div(
      h4(child <-- term.map(_.term.label)),
      dl(
        dt("Synonyms"), dd(child <-- term.map(t => formatSynonyms(t.synonyms))),
        dt("Definition"), dd(child <-- term.map(_.definition.getOrElse(i("None")))),
        dt("ID"), dd(Vocab.compact(iri).id)))
  }

  def entityInfoView(iri: IRI): VNode = {
    val term = KBAPI.termInfo(iri)
    div(
      h4(child <-- term.map(_.term.label)),
      dl(
        dt("Synonyms"), dd(child <-- term.map(t => formatSynonyms(t.synonyms))),
        dt("Definition"), dd(child <-- term.map(_.definition.getOrElse(i("None")))),
        dt("ID"), dd(Vocab.compact(iri).id)),
      p(a(
        href := Util.linkToEntity(iri),
        s"View details for ", span(child <-- term.map(_.term.label)))))
  }

  def taxonInfoView(iri: IRI): VNode = {
    val term = KBAPI.taxon(iri)
    div(
      h4(child <-- term.map(taxonName)),
      dl(
        //dt("Synonyms"), dd(child <-- term.map(t => formatSynonyms(t.))), //FIXME add synonyms to taxon model
        dt("ID"), dd(Vocab.compact(iri).id)),
      p(a(
        href := Util.linkToTaxon(iri),
        s"View details for ", span(child <-- term.map(taxonName)))))
  }

  def publicationInfoView(iri: IRI): VNode = {
    val study = KBAPI.studyInfo(iri)
    div(
      h4(child <-- study.map(_.label)),
      dl(
        dt("Citation"), dd(child <-- study.map(_.citation)),
        dt("ID"), dd(Vocab.compact(iri).id)))
  }

  def pagination(currentPage: Observable[Int], newPage: Sink[Int], totalPages: Observable[Int]): VNode = {
    val onFirstPage = currentPage.map(_ == 1)
    val firstAndPreviousClasses = Util.observableCSS(onFirstPage.map("disabled" -> _))
    val onLastPage = currentPage.combineLatestWith(totalPages)(_ == _)
    val nextAndLastClasses = Util.observableCSS(onLastPage.map("disabled" -> _))
    nav(
      ul(
        cls := "pagination pagination-sm",
        li(cls <-- firstAndPreviousClasses, a(role := "button", click(1) --> newPage, "First")),
        li(cls <-- firstAndPreviousClasses, a(role := "button", click(currentPage.map(_ - 1)) --> newPage, "Previous")),
        li(cls := "active", a(child <-- currentPage)),
        li(cls <-- nextAndLastClasses, a(role := "button", click(currentPage.map(_ + 1)) --> newPage, "Next")),
        li(cls <-- nextAndLastClasses, a(role := "button", click(totalPages) --> newPage, "Last"))))
  }

  val loading: VNode = img(src := "img/ajax-loader.gif") //FIXME verify path

  def autocompleteField[T](search: String => Observable[List[T]], selection: Observable[Option[T]], show: T => String, makeSelection: Sink[Option[T]], placeholderText: Option[String], isDisabled: Observable[Boolean]): VNode = {
    val enteredText = createStringHandler()
    // val selectedIndex = createHandler[Int](0)

    val currentMatches = (for {
      text <- enteredText.filter(_.length > 2).debounceTime(300)
      matches <- search(text)
    } yield matches).merge(selection.map(_ => Nil))
    val hideDropdown = currentMatches.map(_.isEmpty)

    val selectedAsText = selection.map(_.map(show).getOrElse(""))
    val keyHandler = createHandler[String]() //
    //    keyHandler.filter(_ == "down").combineLatestWith(selectedIndex) { (_, index) =>
    //      index + 1
    //    }
    //    val currentIndex = selectedIndex.merge(hideDropdown.map(_ => 0), keyHandler.filter(_ == "down").combineLatestWith(selectedIndex) { (_, index) =>
    //      index + 1
    //    })
    val shouldFocus = createBoolHandler(false) //
    val ulDisplay = hideDropdown.map(if (_) "none" else "block")
    val ulCSS = Util.observableCSS(Observable.of("autocomplete-menu" -> true, "dropdown-menu" -> true).merge(hideDropdown.map("live" -> !_)))
    //keyHandler.map(project)

    def listItem(item: T, index: Int): VNode = li(
      click(Some(item)) --> makeSelection,
      //selected <-- currentIndex.map(_ == index),
      //focus(index) --> selectedIndex,
      role := "option",
      tabindex := 0,
      a(show(item)))

    // div bool classes: has-success has-feedback
    div(
      cls := "form-group",
      style := "position: relative;",
      input(
        tpe := "text",
        cls := "form-control",
        inputString --> enteredText,
        value <-- selectedAsText,
        change.filter(_.target.value.isEmpty)(None) --> makeSelection,
        autocomplete := "off",
        disabled <-- isDisabled,
        placeholder := placeholderText.getOrElse("")),
      ul(
        keydown.filter(_.keyCode == 38)("down") --> keyHandler,
        cls <-- ulCSS,
        role := "menu",
        style := "position: absolute;",
        children <-- currentMatches.map(_.zipWithIndex.map { case (item, i) => listItem(item, i) })))
  }

}