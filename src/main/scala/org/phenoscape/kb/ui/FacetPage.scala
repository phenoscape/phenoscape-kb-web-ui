package org.phenoscape.kb.ui

import org.phenoscape.kb.ui.Model.Facet
import org.phenoscape.kb.ui.Model.IRI
import org.phenoscape.kb.ui.Model.TaxonAnnotation
import org.phenoscape.kb.ui.Model.Term

import outwatch.Sink
import outwatch.dom._
import outwatch.dom.Attributes.style
import outwatch.dom.VNode
import outwatch.redux.Component
import outwatch.redux.Store
import rxscalajs.Observable
import org.phenoscape.kb.ui.Model.AnnotationSource

object FacetPage extends Component {

  sealed trait Action

  final case class SelectTab(tab: FacetTab) extends Action

  final case class SetEntityPath(entity: List[IRI]) extends Action

  final case class SetQualityPath(quality: List[IRI]) extends Action

  final case class SetTaxonPath(taxon: List[IRI]) extends Action

  final case class SetPublication(publication: Option[IRI]) extends Action

  final case class AddEntityToPath(entity: IRI) extends Action

  final case class AddQualityToPath(quality: IRI) extends Action

  final case class AddTaxonToPath(quality: IRI) extends Action

  final case class SetPage(tab: FacetTab, page: Int) extends Action

  final case class SetIncludeParts(include: Boolean) extends Action

  final case class SetIncludeHistoricalHomologs(include: Boolean) extends Action

  final case class SetIncludeSerialHomologs(include: Boolean) extends Action

  final case class SetQualityMode(mode: QualityMode) extends Action

  sealed trait FacetTab

  final case object PhenotypesTab extends FacetTab

  final case object TaxaTab extends FacetTab

  final case object TaxonAnnotationsTab extends FacetTab

  final case object GenesTab extends FacetTab

  final case object GeneAnnotationsTab extends FacetTab

  final case object PublicationsTab extends FacetTab

  sealed trait QualityMode

  final case object PhenotypicQuality extends QualityMode

  final case object InferredPresence extends QualityMode {

    val iri = IRI("http://purl.org/phenoscape/vocab.owl#inferred_presence")

  }

  final case object InferredAbsence extends QualityMode {

    val iri = IRI("http://purl.org/phenoscape/vocab.owl#inferred_absence")

  }

  final case class QuerySpec(entity: Option[IRI], quality: Option[IRI], taxon: Option[IRI], publication: Option[IRI], includeParts: Boolean, includeHistoricalHomologs: Boolean, includeSerialHomologs: Boolean)

  final case class State(
                          selectedTab: FacetTab,
                          selectedEntityPath: List[IRI],
                          selectedQualityPath: List[IRI],
                          selectedTaxonPath: List[IRI],
                          selectedPublication: Option[IRI],
                          qualityMode: QualityMode,
                          includeParts: Boolean,
                          includeHistoricalHomologs: Boolean,
                          includeSerialHomologs: Boolean,
                          taxaPage: Int = 1,
                          phenotypesPage: Int = 1,
                          taxonAnnotationsPage: Int = 1,
                          genesPage: Int = 1,
                          geneAnnotationsPage: Int = 1,
                          publicationsPage: Int = 1) extends ComponentState {

    def evolve: Action => State = {
      case SelectTab(tab)                        => copy(selectedTab = tab)
      case SetEntityPath(entityList)             => copy(selectedEntityPath = entityList, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
      case SetQualityPath(qualityList)           => copy(qualityMode = PhenotypicQuality, selectedQualityPath = qualityList, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
      case SetTaxonPath(taxonList)               => copy(selectedTaxonPath = taxonList, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
      case SetPublication(pubOpt)                => copy(selectedPublication = pubOpt, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, publicationsPage = 1)
      case AddEntityToPath(entity)               => copy(selectedEntityPath = entity :: selectedEntityPath, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
      case AddQualityToPath(quality)             => copy(qualityMode = PhenotypicQuality, selectedQualityPath = quality :: selectedQualityPath, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
      case AddTaxonToPath(taxon)                 => copy(selectedTaxonPath = taxon :: selectedTaxonPath, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
      case SetIncludeParts(include)              => copy(includeParts = include, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
      case SetIncludeHistoricalHomologs(include) => copy(includeHistoricalHomologs = include, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
      case SetIncludeSerialHomologs(include)     => copy(includeSerialHomologs = include, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
      case SetQualityMode(mode)                  => copy(qualityMode = mode, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
      case SetPage(tab, page)                    => tab match {
        case PhenotypesTab       => copy(phenotypesPage = page)
        case TaxaTab             => copy(taxaPage = page)
        case TaxonAnnotationsTab => copy(taxonAnnotationsPage = page)
        case GenesTab            => copy(genesPage = page)
        case GeneAnnotationsTab  => copy(geneAnnotationsPage = page)
        case PublicationsTab     => copy(publicationsPage = page)
      }
    }

    def currentQuerySpec: QuerySpec = {
      val quality = qualityMode match {
        case PhenotypicQuality => selectedQualityPath.headOption
        case InferredPresence  => Some(InferredPresence.iri)
        case InferredAbsence   => Some(InferredAbsence.iri)
      }
      QuerySpec(selectedEntityPath.headOption, quality, selectedTaxonPath.headOption, selectedPublication, includeParts, includeHistoricalHomologs, includeSerialHomologs)
    }

  }

  private type CountFn = Option[IRI] => Observable[Int]

  private type FacetFn = Option[IRI] => Observable[List[Facet]]

  def apply(initState: State): VNode = view(Store.create(Seq.empty, initState))

  def view(store: Store[State, Action]): VNode = {
    val tablePageSize = 40
    val entityPath = store.map(_.selectedEntityPath).distinctUntilChanged
    val querySpecObs = store.map(_.currentQuerySpec).distinctUntilChanged((a, b) => a == b) // Not sure why need to pass this comparison. Perhaps this will be fixed when move to Monix.
    val entity = querySpecObs.map(_.entity).distinctUntilChanged
    val qualityPath = store.map(_.selectedQualityPath).distinctUntilChanged
    val quality = querySpecObs.map(_.quality).distinctUntilChanged
    val entityTerm = entity.flatMap(e => Util.sequence(e.map(KBAPI.termLabel)))
    val qualityTerm = quality.flatMap {
      case Some(InferredPresence.iri) => Observable.of(Some(Term(InferredPresence.iri, "Inferred Presence")))
      case Some(InferredAbsence.iri)  => Observable.of(Some(Term(InferredAbsence.iri, "Inferred Absence")))
      case q                          => Util.sequence(q.map(KBAPI.termLabel))
    }
    val qualityMode = store.map(_.qualityMode).distinctUntilChanged
    val taxonPath = store.map(_.selectedTaxonPath).distinctUntilChanged
    val taxon = querySpecObs.map(_.taxon).distinctUntilChanged
    val taxonTerm = taxon.flatMap(t => Util.sequence(t.map(KBAPI.termLabel)))
    val publication = querySpecObs.map(_.publication).distinctUntilChanged
    val publicationTerm = publication.flatMap(p => Util.sequence(p.map(KBAPI.termLabel)))
    val activeTab = store.map(_.selectedTab).distinctUntilChanged
    val obsTaxonPage = store.map(_.taxaPage).distinctUntilChanged
    val obsPhenotypesPage = store.map(_.phenotypesPage).distinctUntilChanged
    val obsAnnotationsPage = store.map(_.taxonAnnotationsPage).distinctUntilChanged
    val obsPublicationsPage = store.map(_.publicationsPage).distinctUntilChanged
    val obsGenePage = store.map(_.genesPage).distinctUntilChanged
    val tableTitle = activeTab.map {
      case PhenotypesTab       => "Phenotypes"
      case TaxonAnnotationsTab => "Taxon annotations"
      case TaxaTab             => "Taxa with phenotype"
      case GeneAnnotationsTab  => "Gene annotations"
      case GenesTab            => "Genes"
      case PublicationsTab     => "Publications"
    }

    def totalToPages(num: Int): Int = (num / tablePageSize.toDouble).ceil.toInt

    val phenotypesTotalObs = querySpecObs.flatMap {
      case QuerySpec(e, q, t, p, parts, hist, serial) =>
        KBAPI.countTaxonPhenotypes(e, q, t, p, parts, hist, serial).map(Option(_)).startWith(None)
    }
    val phenotypesTotalPagesObs = phenotypesTotalObs.map(_.map(totalToPages))
    val taxaTotalObs = querySpecObs.flatMap {
      case QuerySpec(e, q, t, p, parts, hist, serial) =>
        KBAPI.countTaxaWithPhenotype(e, q, t, p, parts, hist, serial).map(Option(_)).startWith(None)
    }
    val taxaTotalPagesObs = taxaTotalObs.map(_.map(totalToPages))
    val annotationsTotalObs = querySpecObs.flatMap {
      case QuerySpec(e, q, t, p, parts, hist, serial) =>
        KBAPI.countTaxonAnnotations(e, q, t, p, parts, hist, serial).map(Option(_)).startWith(None)
    }
    val annotationsTotalPagesObs = annotationsTotalObs.map(_.map(totalToPages))
    val publicationsTotalObs = querySpecObs.flatMap {
      case QuerySpec(e, q, t, p, parts, hist, serial) =>
        KBAPI.countStudiesWithPhenotype(e, q, t, p, parts, hist, serial).map(Option(_)).startWith(None)
    }
    val publicationsTotalPagesObs = publicationsTotalObs.map(_.map(totalToPages))
    val genesTotalObs = querySpecObs.flatMap {
      case QuerySpec(e, q, _, p, parts, hist, serial) =>
        KBAPI.countGenesWithPhenotype(e, q, parts, hist, serial).map(Option(_)).startWith(None)
    }
    val genesTotalPagesObs = genesTotalObs.map(_.map(totalToPages))
    val currentPagesObs = store.map(s => Map[FacetTab, Int](
      PhenotypesTab -> s.phenotypesPage,
      TaxaTab -> s.taxaPage,
      TaxonAnnotationsTab -> s.taxonAnnotationsPage,
      PublicationsTab -> s.publicationsPage,
      GenesTab -> s.genesPage,
      GeneAnnotationsTab -> s.geneAnnotationsPage)).distinctUntilChanged
    val tableWithData = activeTab.combineLatestWith(querySpecObs, currentPagesObs)(dataTable(_, _, _, tablePageSize))
    val entityCountFn = querySpecObs.map(spec => (spec.quality, spec.taxon, spec.publication, spec.includeParts, spec.includeHistoricalHomologs, spec.includeSerialHomologs)).distinctUntilChanged.combineLatestWith(activeTab) {
      case ((q, t, p, parts, hist, serial), tab) =>
        tab match {
          case PhenotypesTab       => KBAPI.countTaxonPhenotypes(_: Option[IRI], q, t, p, parts, hist, serial)
          case TaxaTab             => KBAPI.countTaxaWithPhenotype(_: Option[IRI], q, t, p, parts, hist, serial)
          case GenesTab            => KBAPI.countGenesWithPhenotype(_: Option[IRI], q, parts, hist, serial)
          case TaxonAnnotationsTab => KBAPI.countTaxonAnnotations(_: Option[IRI], q, t, p, parts, hist, serial)
          //          case GeneAnnotationsTab  => KBAPI.countGeneAnnotations()
          case PublicationsTab => KBAPI.countStudiesWithPhenotype(_: Option[IRI], q, t, p, parts, hist, serial)
        }
    }
    val entityFacetFn = querySpecObs.map(spec => (spec.quality, spec.taxon, spec.publication, spec.includeParts, spec.includeHistoricalHomologs, spec.includeSerialHomologs)).distinctUntilChanged.combineLatestWith(activeTab) {
      case ((q, t, p, parts, hist, serial), tab) =>
        tab match {
          case PhenotypesTab       => KBAPI.facetTaxonPhenotypes("entity", _: Option[IRI], q, t, p, parts, hist, serial)
          case TaxaTab             => KBAPI.facetTaxaWithPhenotype("entity", _: Option[IRI], q, t, p, parts, hist, serial)
          case GenesTab            => KBAPI.facetGenesWithPhenotype("entity", _: Option[IRI], q, parts, hist, serial)
          case TaxonAnnotationsTab => KBAPI.facetTaxonAnnotations("entity", _: Option[IRI], q, t, p, parts, hist, serial)
          case PublicationsTab     => KBAPI.facetStudiesWithPhenotype("entity", _: Option[IRI], q, t, p, parts, hist, serial)
        }
    }
    val qualityCountFn = querySpecObs.map(spec => (spec.entity, spec.taxon, spec.publication, spec.includeParts, spec.includeHistoricalHomologs, spec.includeSerialHomologs)).distinctUntilChanged.combineLatestWith(activeTab) {
      case ((e, t, p, parts, hist, serial), tab) =>
        tab match {
          case PhenotypesTab       => KBAPI.countTaxonPhenotypes(e, _: Option[IRI], t, p, parts, hist, serial)
          case TaxaTab             => KBAPI.countTaxaWithPhenotype(e, _: Option[IRI], t, p, parts, hist, serial)
          case GenesTab            => KBAPI.countGenesWithPhenotype(e, _: Option[IRI], parts, hist, serial)
          case TaxonAnnotationsTab => KBAPI.countTaxonAnnotations(e, _: Option[IRI], t, p, parts, hist, serial)
          case PublicationsTab     => KBAPI.countStudiesWithPhenotype(e, _: Option[IRI], t, p, parts, hist, serial)
        }
    }
    val qualityFacetFn = querySpecObs.map(spec => (spec.entity, spec.taxon, spec.publication, spec.includeParts, spec.includeHistoricalHomologs, spec.includeSerialHomologs)).distinctUntilChanged.combineLatestWith(activeTab) {
      case ((e, t, p, parts, hist, serial), tab) =>
        tab match {
          case PhenotypesTab       => KBAPI.facetTaxonPhenotypes("quality", e, _: Option[IRI], t, p, parts, hist, serial)
          case TaxaTab             => KBAPI.facetTaxaWithPhenotype("quality", e, _: Option[IRI], t, p, parts, hist, serial)
          case GenesTab            => KBAPI.facetGenesWithPhenotype("quality", e, _: Option[IRI], parts, hist, serial)
          case TaxonAnnotationsTab => KBAPI.facetTaxonAnnotations("quality", e, _: Option[IRI], t, p, parts, hist, serial)
          case PublicationsTab     => KBAPI.facetStudiesWithPhenotype("quality", e, _: Option[IRI], t, p, parts, hist, serial)
        }
    }
    val taxonCountFn = querySpecObs.map(spec => (spec.entity, spec.quality, spec.publication, spec.includeParts, spec.includeHistoricalHomologs, spec.includeSerialHomologs)).distinctUntilChanged.combineLatestWith(activeTab) {
      case ((e, q, p, parts, hist, serial), tab) =>
        tab match {
          case PhenotypesTab       => KBAPI.countTaxonPhenotypes(e, q, _: Option[IRI], p, parts, hist, serial)
          case TaxaTab             => KBAPI.countTaxaWithPhenotype(e, q, _: Option[IRI], p, parts, hist, serial)
          case TaxonAnnotationsTab => KBAPI.countTaxonAnnotations(e, q, _: Option[IRI], p, parts, hist, serial)
          case PublicationsTab     => KBAPI.countStudiesWithPhenotype(e, q, _: Option[IRI], p, parts, hist, serial)
          case GenesTab            => (_: Option[IRI]) => Observable.of(0)
        }
    }
    val taxonFacetFn = querySpecObs.map(spec => (spec.entity, spec.quality, spec.publication, spec.includeParts, spec.includeHistoricalHomologs, spec.includeSerialHomologs)).distinctUntilChanged.combineLatestWith(activeTab) {
      case ((e, q, p, parts, hist, serial), tab) =>
        tab match {
          case PhenotypesTab       => KBAPI.facetTaxonPhenotypes("taxon", e, q, _: Option[IRI], p, parts, hist, serial)
          case TaxaTab             => KBAPI.facetTaxaWithPhenotype("taxon", e, q, _: Option[IRI], p, parts, hist, serial)
          case TaxonAnnotationsTab => KBAPI.facetTaxonAnnotations("taxon", e, q, _: Option[IRI], p, parts, hist, serial)
          case PublicationsTab     => KBAPI.facetStudiesWithPhenotype("taxon", e, q, _: Option[IRI], p, parts, hist, serial)
          case GenesTab            => (_: Option[IRI]) => Observable.of(Nil)
        }
    }
    val setEntityPath: Sink[List[IRI]] = store.sink.redirectMap(SetEntityPath)
    val setQualityPath: Sink[List[IRI]] = store.sink.redirectMap(SetQualityPath)
    val setTaxonPath: Sink[List[IRI]] = store.sink.redirectMap(SetTaxonPath)
    val setPublicationOpt: Sink[Option[IRI]] = store.sink.redirectMap(SetPublication)
    val entitySearch = Views.autocompleteField(KBAPI.ontologyClassSearch(_: String, Some(IRI(Vocab.Uberon)), 20), entityTerm, (term: Term) => term.label, setEntityPath.redirectMap((ot: Option[Term]) => ot.map(_.iri).toList), Some("any anatomical entity"), Observable.of(false))
    val qualitySearch = Views.autocompleteField(KBAPI.ontologyClassSearch(_: String, Some(IRI(Vocab.PATO)), 20), qualityTerm, (term: Term) => term.label, setQualityPath.redirectMap((ot: Option[Term]) => ot.map(_.iri).toList), Some("any phenotypic quality"), qualityMode.map(_ != PhenotypicQuality))
    val taxonSearch = Views.autocompleteField(KBAPI.ontologyClassSearch(_: String, Some(IRI(Vocab.VTO)), 20), taxonTerm, (term: Term) => term.label, setTaxonPath.redirectMap((ot: Option[Term]) => ot.map(_.iri).toList), Some("any taxonomic group"), Observable.of(false))
    val pubSearch = Views.autocompleteField(KBAPI.studySearch, publicationTerm, (term: Term) => term.label, setPublicationOpt.redirectMap((ot: Option[Term]) => ot.map(_.iri)), Some("any publication"), Observable.of(false))

    div(
      cls := "row",
      div(
        cls := "col-sm-4",
        div(
          cls := "panel panel-default facet-controls query-panel",
          style := "margin-top: 1em;",
          div(
            cls := "panel-heading",
            h4(cls := "panel-title", "Query")),
          div(
            cls := "panel-body",
            entitySearch,
            qualitySearch,
            taxonSearch,
            pubSearch)),
        facetControls("anatomical entity", entityPath, entityCountFn, entityFacetFn, setEntityPath, Observable.of(false), Observable.of(false))(Some(div(
          cls := "form-inline",
          div(
            div(cls := "form-group", label("Include: ")),
            div(cls := "checkbox", label(input(
              tpe := "checkbox",
              checked <-- store.map(_.includeParts),
              inputChecked --> store.sink.redirectMap(SetIncludeParts)), " parts ")),
            div(cls := "checkbox", label(input(
              tpe := "checkbox",
              checked <-- store.map(_.includeHistoricalHomologs),
              inputChecked --> store.sink.redirectMap(SetIncludeHistoricalHomologs)), " historical homologs ")),
            div(cls := "checkbox", label(input(
              tpe := "checkbox",
              checked <-- store.map(_.includeSerialHomologs),
              inputChecked --> store.sink.redirectMap(SetIncludeSerialHomologs)), " serial homologs ")))))),
        facetControls("phenotypic quality", qualityPath, qualityCountFn, qualityFacetFn, setQualityPath, Observable.of(false), qualityMode.map(_ != PhenotypicQuality))(Some(div(
          cls := "form-inline",
          div(
            div(cls := "form-group", label("Mode: ")),
            div(cls := "radio", label(input(
              tpe := "radio",
              name := "phenotypic-quality-radio",
              checked <-- qualityMode.map(_ == PhenotypicQuality),
              inputChecked --> store.sink.redirectMap(_ => SetQualityMode(PhenotypicQuality))), " quality ")),
            div(cls := "radio", label(input(
              tpe := "radio",
              name := "phenotypic-quality-radio",
              checked <-- qualityMode.map(_ == InferredPresence),
              inputChecked --> store.sink.redirectMap(_ => SetQualityMode(InferredPresence))), " inferred presence ")),
            div(cls := "radio", label(input(
              tpe := "radio",
              name := "phenotypic-quality-radio",
              checked <-- qualityMode.map(_ == InferredAbsence),
              inputChecked --> store.sink.redirectMap(_ => SetQualityMode(InferredAbsence))), " inferred absence ")))))),
        facetControls("taxonomic group", taxonPath, taxonCountFn, taxonFacetFn, setTaxonPath, activeTab.map(t => Set[FacetTab](GenesTab, GeneAnnotationsTab)(t)), Observable.of(false))(None)),
      div(
        cls := "col-sm-8",
        ul(
          cls := "nav nav-tabs",
          li(role := "presentation", cls <-- Util.observableCSS(activeTab.map(t => "active" -> (t == PhenotypesTab))),
            a(click(SelectTab(PhenotypesTab)) --> store, role := "button", "Phenotypes ", span(cls := "badge", child <-- phenotypesTotalObs.map(_.getOrElse(""))))),
          li(role := "presentation", cls <-- Util.observableCSS(activeTab.map(t => "active" -> (t == TaxonAnnotationsTab))),
            a(click(SelectTab(TaxonAnnotationsTab)) --> store, role := "button", "Taxon annotations ", span(cls := "badge", child <-- annotationsTotalObs.map(_.getOrElse(""))))),
          li(role := "presentation", cls <-- Util.observableCSS(activeTab.map(t => "active" -> (t == TaxaTab))),
            a(click(SelectTab(TaxaTab)) --> store, role := "button", "Taxa ", span(cls := "badge", child <-- taxaTotalObs.map(_.getOrElse(""))))),
          li(role := "presentation", cls <-- Util.observableCSS(activeTab.map(t => "active" -> (t == PublicationsTab))),
            a(click(SelectTab(PublicationsTab)) --> store, role := "button", "Publications ", span(cls := "badge", child <-- publicationsTotalObs.map(_.getOrElse(""))))),
          li(role := "presentation", cls <-- Util.observableCSS(activeTab.map(t => "active" -> (t == GenesTab))),
            a(click(SelectTab(GenesTab)) --> store, role := "button", "Genes ", span(cls := "badge", child <-- genesTotalObs.map(_.getOrElse("")))))),
        div(
          cls := "panel panel-default",
          style := "margin-top: 1em;",
          div(
            cls := "panel-heading",
            h4(cls := "panel-title", child <-- tableTitle)),
          div(
            cls := "panel-body"),
          child <-- tableWithData),
        div(hidden <-- activeTab.map(_ != TaxaTab), Views.pagination(obsTaxonPage, store.redirectMap(SetPage(TaxaTab, _)), taxaTotalPagesObs.map(_.getOrElse(0)))),
        div(hidden <-- activeTab.map(_ != PhenotypesTab), Views.pagination(obsPhenotypesPage, store.redirectMap(SetPage(PhenotypesTab, _)), phenotypesTotalPagesObs.map(_.getOrElse(0)))),
        div(hidden <-- activeTab.map(_ != TaxonAnnotationsTab), Views.pagination(obsAnnotationsPage, store.redirectMap(SetPage(TaxonAnnotationsTab, _)), annotationsTotalPagesObs.map(_.getOrElse(0)))),
        div(hidden <-- activeTab.map(_ != PublicationsTab), Views.pagination(obsPublicationsPage, store.redirectMap(SetPage(PublicationsTab, _)), publicationsTotalPagesObs.map(_.getOrElse(0)))),
        div(hidden <-- activeTab.map(_ != GenesTab), Views.pagination(obsGenePage, store.redirectMap(SetPage(GenesTab, _)), genesTotalPagesObs.map(_.getOrElse(0))))))
  }

  private def dataTable(tab: FacetTab, querySpec: QuerySpec, currentPages: Map[FacetTab, Int], tableSize: Int): VNode = {
    val QuerySpec(entity, quality, taxon, publication, parts, hist, serial) = querySpec

    def offset(page: Int) = tableSize * (page - 1).max(0)

    val (header, rows) = tab match {
      case PhenotypesTab       =>
        val data = KBAPI.queryTaxonPhenotypes(entity, quality, taxon, publication, parts, hist, serial, tableSize, offset(currentPages(PhenotypesTab))).startWith(Nil)
        (
          thead(tr(th("Phenotype"))),
          tbody(
            cls := "striped",
            children <-- data.map(_.map(singleTermRow))))
      case TaxaTab             =>
        val data = KBAPI.queryTaxaWithPhenotype(entity, quality, taxon, publication, parts, hist, serial, tableSize, offset(currentPages(TaxaTab))).startWith(Nil)
        (
          thead(tr(
            th("Group"),
            th("Taxon"))),
          tbody(
            cls := "striped",
            children <-- data.map(_.map(taxonRow))))
      case TaxonAnnotationsTab =>
        val data = KBAPI.queryTaxonAnnotations(entity, quality, taxon, publication, parts, hist, serial, tableSize, offset(currentPages(TaxonAnnotationsTab))).startWith(Nil)
        (
          thead(tr(
            th("Group"),
            th("Taxon"),
            th("Phenotype"),
            th("Sources"))),
          tbody(
            cls := "sibling-striped",
            children <-- data.map(_.flatMap(taxonAnnotationRow))))
      case PublicationsTab     =>
        val data = KBAPI.queryStudiesWithPhenotype(entity, quality, taxon, publication, parts, hist, serial, tableSize, offset(currentPages(PublicationsTab))).startWith(Nil)
        (
          thead(tr(
            th("Publication"))),
          tbody(
            cls := "striped",
            children <-- data.map(_.map(publicationRow))))
      case GenesTab            =>
        val data = KBAPI.queryGenesWithPhenotype(entity, quality, parts, hist, serial, tableSize, offset(currentPages(GenesTab))).startWith(Nil)
        (
          thead(tr(
            th("Organism"),
            th("Gene"))),
          tbody(
            cls := "striped",
            children <-- data.map(_.map(geneRow))))
      case GeneAnnotationsTab  => ???
    }
    table(
      cls := "table table-condensed", header, rows)
  }

  private def singleTermRow(term: Term): VNode = {
    tr(td(term.label))
  }

  private def taxonRow(term: Term): VNode = {
    val taxonName = KBAPI.taxon(term.iri).map(Views.taxonName)
    val group = KBAPI.taxonCommonGroup(term.iri).map { grp =>
      val thumbnail = Util.taxonThumbnailIRI(grp.phylopic).id
      span(Popover.simplePopover, cls := "common-taxon-group", data.toggle := "popover", data.trigger := "hover", data.placement := "right", data.content := grp.label,
        img(src := thumbnail))
    } //FIXME repeated code with similarity page
    tr(
      cls := "striped",
      td(child <-- group),
      td(Popover.popup(Views.taxonInfoView(term.iri), "auto", "focus")(child <-- taxonName.startWith(term.label))))
  }

  private def taxonAnnotationRow(annotation: TaxonAnnotation): Seq[VNode] = {
    val taxonName = KBAPI.taxon(annotation.taxon.iri).map(Views.taxonName)
    val group = KBAPI.taxonCommonGroup(annotation.taxon.iri).map { grp =>
      val thumbnail = Util.taxonThumbnailIRI(grp.phylopic).id
      span(Popover.simplePopover, cls := "common-taxon-group", data.toggle := "popover", data.trigger := "hover", data.placement := "right", data.content := grp.label,
        img(src := thumbnail))
    } //FIXME repeated code with similarity page
    val siblingToggleClickHandler = createHandler[Unit]()
    val innerView = siblingToggleClickHandler.first.map(_ => KBAPI.annotationSources(annotation).map(_.map(annotationSourceView))).flatten
    val shouldHideSiblingView = siblingToggleClickHandler.scan(0)((a, _) => a + 1).map(i => (i % 2) == 0).startWith(true)
    Seq(
      tr(
        cls := "striped",
        td(child <-- group),
        td(Popover.popup(Views.taxonInfoView(annotation.taxon.iri), "auto", "focus")(child <-- taxonName.startWith(annotation.taxon.label))),
        td(annotation.phenotype.label),
        td(a(
          role := "button",
          click(()) --> siblingToggleClickHandler,
          span(cls := "glyphicon glyphicon-list-alt")))),
      tr(
        hidden <-- shouldHideSiblingView,
        td(
          colspan := 4,
          div(
            cls := "well",
            h4("Source data"),
            div(children <-- innerView)))))
  }

  private def annotationSourceView(as: AnnotationSource): VNode = {
    div(
      h5(Popover.popup(Views.publicationInfoView(as.study.iri), "auto", "focus")(as.study.label)),
      dl(
        cls := "dl-horizontal",
        dt(s"Character ${as.characterNum}:"),
        dd(as.characterText),
        dt("State:"),
        dd(as.stateText)))
  }

  private def geneRow(term: Term): VNode = {
    val thumbnail = Util.modelOrganismThumbnailURL(term.iri)
    tr(
      td(span(cls := "common-taxon-group", img(src := thumbnail))),
      td(a(href := Util.linkToGene(term.iri), term.label)))
  }

  private def publicationRow(term: Term): VNode = {
    tr(td(Popover.popup(Views.publicationInfoView(term.iri), "auto", "focus")(term.label)))
  }

  private def facetControls(title: String, focusItemPath: Observable[List[IRI]], countFunc: Observable[CountFn], facetFunc: Observable[FacetFn], newFocus: Sink[List[IRI]], disabledObs: Observable[Boolean], facetsHiddenObs: Observable[Boolean])(accessoryView: Option[VNode]): VNode = {
    val facetPathPaths = focusItemPath.map { list =>
      list.reverse.scanLeft(List.empty[IRI])((path, term) => term :: path).drop(1)
    }
    val facetPathElements = facetPathPaths.map { paths =>
      val lastIndex = paths.size - 1
      paths.zipWithIndex.map {
        case (path, index) =>
          facetPathLink(path, countFunc, newFocus, index == lastIndex)
      }
    }.startWith(Nil)
    val facetDataAndStatus = focusItemPath.combineLatestWith(facetFunc) { (list, facetFn) =>
      facetFn(list.headOption).map(x => (x, list) -> true).startWith((Nil, list) -> false)
    }.flatten
    val facetChildElements = facetDataAndStatus.map {
      case ((facets, iris), _) =>
        facets.sortBy(-_.count).map(facetChildLink(_, iris, newFocus))
    }
    val childrenLoaded = facetDataAndStatus.map(_._2)
    val anyCount = countFunc.flatMap(_ (None).map(_.toString).startWith(""))
    val anySelected = focusItemPath.map(_.isEmpty)
    val anyCSSClass = anySelected.map(flag => if (flag) "selected-facet" else "")

    div(
      cls := "panel panel-default facet-controls",
      style := "margin-top: 1em;",
      div(
        cls := "panel-heading",
        h4(cls := "panel-title", title.capitalize)),
      div(
        hidden <-- disabledObs,
        cls := "panel-body",
        div(cls := "facet-accessory", accessoryView.getOrElse(div())),
        div(
          hidden <-- facetsHiddenObs,
          div(cls := "facet-line", a(role := "button", cls <-- anyCSSClass, click(Nil) --> newFocus, s"Any $title"), " ", span(cls := "badge", child <-- anyCount)),
          div(children <-- facetPathElements),
          div(children <-- facetChildElements),
          div(hidden <-- childrenLoaded, Views.loading))))
  }

  private def facetPathLink(termPath: List[IRI], countFunc: Observable[CountFn], newFocus: Sink[List[IRI]], selected: Boolean): VNode = {
    val currentTerm = termPath.head
    val termLabelObs = KBAPI.termLabel(currentTerm).map(_.label)
    val countValue = countFunc.flatMap(_ (Some(currentTerm)))
    val countLoaded = countValue.map(_ => true).startWith(false)
    val count = countValue.map(_.toString).startWith("")
    val indent = span(termPath.map(_ => span(cls := "facet-indent")): _*)
    val cssClass = if (selected) "selected-facet" else ""
    div(cls := "facet-line", indent,
      Popover.popup(Views.termInfoView(currentTerm), "right", "hover")(cls := cssClass, click(termPath) --> newFocus, child <-- termLabelObs),
      " ", span(hidden <-- countLoaded, Views.loading), span(cls := "badge", child <-- count))
  }

  private def facetChildLink(facetItem: Facet, termPath: List[IRI], newFocus: Sink[List[IRI]]): VNode = {
    val newPath = facetItem.term.iri :: termPath
    val indent = span(newPath.map(_ => span(cls := "facet-indent")): _*)
    div(cls := "facet-line", indent,
      Popover.popup(Views.termInfoView(facetItem.term.iri), "right", "hover")(click(newPath) --> newFocus, facetItem.term.label),
      " ", span(cls := "badge", facetItem.count.toString))
  }

}