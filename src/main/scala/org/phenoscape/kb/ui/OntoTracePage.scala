package org.phenoscape.kb.ui

import org.phenoscape.kb.ui.Model.IRI
import org.phenoscape.kb.ui.Model.Term
import org.phenoscape.kb.ui.Model.Taxon
import org.phenoscape.kb.ui.Views.taxonName
import org.phenoscape.kb.ui.Vocab._
import org.phenoscape.kb.ui.Util.StringOps

import outwatch.dom.VNode
import outwatch.redux.Component
import outwatch.redux.Store
import rxscalajs.Observable
import cats.data.Validated
import outwatch.Sink
import cats.data.Validated.Invalid
import cats.data.Validated.Valid

object OntoTracePage extends Component {

  sealed trait Action
  final case class SetMode(mode: InputMode) extends Action
  final case class SetTaxonIRI(taxon: Option[IRI]) extends Action
  final case class SetEntityIRI(entity: Option[IRI]) extends Action
  final case class SetIncludeParts(include: Boolean) extends Action
  final case class SetIncludeAllCharacters(include: Boolean) extends Action
  final case class SetTaxonExpression(taxon: Validated[Option[String], String]) extends Action
  final case class SetEntityExpression(entity: Validated[Option[String], String]) extends Action

  sealed trait InputMode
  final case object SimpleMode extends InputMode
  final case object ExpressionMode extends InputMode

  case class State(
    mode:                 InputMode,
    taxonIRIOpt:          Option[IRI],
    entityIRIOpt:         Option[IRI],
    includeParts:         Boolean,
    includeAllCharacters: Boolean,
    taxonExpression:      Validated[Option[String], String],
    entityExpression:     Validated[Option[String], String]) extends ComponentState {

    def evolve = {
      case SetMode(newMode)                 => copy(mode = newMode)
      case SetTaxonIRI(maybeIRI)            => copy(taxonIRIOpt = maybeIRI)
      case SetEntityIRI(maybeIRI)           => copy(entityIRIOpt = maybeIRI)
      case SetIncludeParts(include)         => copy(includeParts = include)
      case SetIncludeAllCharacters(include) => copy(includeAllCharacters = include)
      case SetTaxonExpression(maybeExp)     => copy(taxonExpression = maybeExp)
      case SetEntityExpression(maybeExp)    => copy(entityExpression = maybeExp)
    }

    def downloadURL: Option[String] = {
      val (taxonOpt, entityOpt) = mode match {
        case SimpleMode     => (taxonIRIOpt.map(iri => s"<${iri.id}>"), entityIRIOpt.map(iri => s"<${iri.id}>"))
        case ExpressionMode => (taxonExpression.toOption, entityExpression.toOption)
      }
      for {
        taxon <- taxonOpt
        entity <- entityOpt
      } yield KBAPI.ontotraceURL(taxon, entity, includeParts, !includeAllCharacters)
    }

  }

  def apply(initState: State): VNode = {
    view(Store.create(Seq.empty, initState))
  }

  def view(store: Store[State, Action]): VNode = {
    import outwatch.dom._
    import outwatch.dom.Attributes.title

    val showSimpleMode = store.map(_.mode == SimpleMode).distinctUntilChanged
    val setMode = store.sink.redirectMap(SetMode(_))

    val simpleTaxonTermObs = store.map(_.taxonIRIOpt).distinctUntilChanged.flatMap(iriOpt => Util.sequence(iriOpt.map(KBAPI.termLabel)))
    val simpleEntityTermObs = store.map(_.entityIRIOpt).distinctUntilChanged.flatMap(iriOpt => Util.sequence(iriOpt.map(KBAPI.termLabel)))

    val ontotraceDownloadURL = store.map(_.downloadURL).distinctUntilChanged

    val taxonExpressionObs = store.map(_.taxonExpression).distinctUntilChanged
    val invalidTaxonError = taxonExpressionObs.map(_.swap.toOption.flatten)
    val hasTaxonExpressionFeedback = taxonExpressionObs.map {
      case Valid(_)         => true
      case Invalid(Some(_)) => true
      case Invalid(None)    => false
    }
    val hasValidTaxonExpression = taxonExpressionObs.map(_.isValid)
    val taxonFormCSS = Util.observableCSS(hasTaxonExpressionFeedback.map("has-feedback" -> _).merge(hasValidTaxonExpression.map("has-success" -> _)).startWith("form-group" -> true))
    val enterTaxonExpression = store.sink.redirect { labelExp: Observable[String] =>
      for {
        enteredText <- labelExp.debounceTime(500)
        maybeText = enteredText.stripToOption
        maybeResolved <- Util.sequence(maybeText.map(KBAPI.resolveLabelExpression))
      } yield SetTaxonExpression(maybeResolved match {
        case Some(validated) => validated.leftMap(Option(_))
        case None            => Invalid(None)
      })
    }

    val entityExpressionObs = store.map(_.entityExpression).distinctUntilChanged
    val invalidEntityError = entityExpressionObs.map(_.swap.toOption.flatten)
    val hasEntityExpressionFeedback = entityExpressionObs.map {
      case Valid(_)         => true
      case Invalid(Some(_)) => true
      case Invalid(None)    => false
    }
    val hasValidEntityExpression = entityExpressionObs.map(_.isValid)
    val entityFormCSS = Util.observableCSS(hasEntityExpressionFeedback.map("has-feedback" -> _).merge(hasValidEntityExpression.map("has-success" -> _)).startWith("form-group" -> true))
    val enterEntityExpression = store.sink.redirect { labelExp: Observable[String] =>
      for {
        enteredText <- labelExp.debounceTime(500)
        maybeText = enteredText.stripToOption
        maybeResolved <- Util.sequence(maybeText.map(KBAPI.resolveLabelExpression))
      } yield SetEntityExpression(maybeResolved match {
        case Some(validated) => validated.leftMap(Option(_))
        case None            => Invalid(None)
      })
    }

    div(
      h2("OntoTrace"),
      p("Use the OntoTrace query to download a character-by-taxon matrix containing both asserted and inferred presence/absence values for specified kinds of anatomical entities and taxa."),
      div(cls := "alert alert-info", role := "alert",
        p(cls := "bg-info", """Matrices generated via the OntoTrace web interface do not include embedded annotations about the asserted published character states entailing each inference. 
          To obtain an OntoTrace file with this embedded metadata (often much larger in size), please contact Jim Balhoff at balhoff@renci.org.""")),
      div(
        cls := "panel panel-default",
        div(
          cls := "panel-body",
          form(
            role := "form",
            div(
              cls := "btn-group",
              label(
                cls <-- Util.observableCSS(showSimpleMode.map("active" -> _).startWithMany("btn" -> true, "btn-default" -> true)),
                click(SimpleMode) --> setMode,
                "Simple Input"),
              label(
                cls <-- Util.observableCSS(showSimpleMode.map("active" -> !_).startWithMany("btn" -> true, "btn-default" -> true)),
                click(ExpressionMode) --> setMode,
                "OWL Expression Input")),
            div(
              hidden <-- showSimpleMode.map(!_),
              p(cls := "top-buffer", "Choose a taxonomic group and type of anatomical structure using the autocomplete fields."),
              div(
                h4("Taxon is:"),
                div(
                  cls := "form-group",
                  Views.autocompleteField(KBAPI.ontologyClassSearch(_: String, Some(IRI(Vocab.VTO)), 20), simpleTaxonTermObs, (term: Term) => term.label, store.sink.redirectMap((ot: Option[Term]) => SetTaxonIRI(ot.map(_.iri))), Some("any taxonomic group"), Observable.of(false))
                //span(cls := "glyphicon glyphicon-ok form-control-feedback")
                )),
              h5(i("and")),
              div(
                h4("Entity is:"),
                div(
                  cls := "form-group",
                  Views.autocompleteField(KBAPI.ontologyClassSearch(_: String, Some(IRI(Vocab.Uberon)), 20), simpleEntityTermObs, (term: Term) => term.label, store.sink.redirectMap((ot: Option[Term]) => SetEntityIRI(ot.map(_.iri))), Some("any anatomical entity"), Observable.of(false))
                //span(cls := "glyphicon glyphicon-ok form-control-feedback")
                )),
              div(
                label(
                  input(
                    tpe := "checkbox",
                    checked <-- store.map(_.includeParts),
                    inputChecked --> store.sink.redirectMap(SetIncludeParts)),
                  " Include parts of ",
                  span(
                    hidden <-- simpleEntityTermObs.map(_.nonEmpty),
                    "entity"),
                  span(
                    hidden <-- simpleEntityTermObs.map(_.isEmpty),
                    mark(child <-- simpleEntityTermObs.map(_.map(_.label).getOrElse(""))))))),
            div(
              hidden <-- showSimpleMode,
              p(cls := "top-buffer", "Enter taxonomic and anatomical expressions using term labels in OWL Manchester syntax (as in ",
                a(href := "http://protege.stanford.edu", "Protégé"),
                "). Labels containing spaces must be single-quoted, e.g. ", mark("'basihyal bone'"),
                ". Exact label matches will be resolved to term identifiers by the Knowledgebase."),
              p("Example:"),
              div(
                cls := "well",
                dl(
                  dt("Taxon"),
                  dd("Characiformes or Siluriformes"),
                  dt("Entity"),
                  dd("'skeletal element' and ('part of' some fin)"))),
              div(
                h4("Taxon is:"),
                div(
                  cls <-- taxonFormCSS,
                  input(
                    title <-- store.map(_.taxonExpression.fold(_ => "invalid expression", identity)),
                    tpe := "text",
                    placeholder := "taxonomic group expression",
                    cls := "form-control",
                    inputString --> enterTaxonExpression) //,
                //span(hidden <-- hasValidTaxonExpression.map(!_), cls := "glyphicon glyphicon-ok form-control-feedback", Aria.hidden := true),
                //span(hidden <-- invalidTaxonError.map(_.isEmpty), cls := "glyphicon glyphicon-remove form-control-feedback", Aria.hidden := true)
                ),
                div(
                  hidden <-- invalidTaxonError.map(_.isEmpty),
                  cls := "alert alert-danger", role := "alert",
                  child <-- invalidTaxonError.map(_.getOrElse("")))),
              h5(i("and")),
              div(
                h4("Entity is:"),
                div(
                  cls <-- entityFormCSS,
                  input(
                    title <-- store.map(_.entityExpression.fold(_ => "invalid expression", identity)),
                    tpe := "text",
                    placeholder := "anatomical entity expression",
                    cls := "form-control",
                    inputString --> enterEntityExpression) //,
                //span(hidden <-- hasValidEntityExpression.map(!_), cls := "glyphicon glyphicon-ok form-control-feedback", Aria.hidden := true),
                //span(hidden <-- invalidEntityError.map(_.isEmpty), cls := "glyphicon glyphicon-remove form-control-feedback", Aria.hidden := true)
                ),
                div(
                  hidden <-- invalidEntityError.map(_.isEmpty),
                  cls := "alert alert-danger", role := "alert",
                  child <-- invalidEntityError.map(_.getOrElse(""))))),
            div(
              label(
                input(
                  tpe := "checkbox",
                  checked <-- store.map(_.includeAllCharacters),
                  inputChecked --> store.sink.redirectMap(SetIncludeAllCharacters)),
                " Include all characters with values (only variable characters are included by default—those with data for both presence and absence)")),
            a(
              cls := "btn btn-primary btn-block",
              href <-- ontotraceDownloadURL.map(_.getOrElse("")),
              disabled <-- ontotraceDownloadURL.map(_.isEmpty),
              "Download matrix as NeXML")))))
  }

}