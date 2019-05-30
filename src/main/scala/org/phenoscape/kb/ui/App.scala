package org.phenoscape.kb.ui

import cats.data.Validated.Invalid
import org.phenoscape.kb.ui.FacetPage.PhenotypicQuality
import org.phenoscape.kb.ui.Model.{Curie, IRI}
import org.phenoscape.kb.ui.QueryParams._
import outwatch.dom._
import outwatch.router.{BaseUrl, Router}
import rxscalajs.Observable

import scala.scalajs.js.JSApp
import scala.scalajs.js.URIUtils.decodeURIComponent

object App extends JSApp {

  def main(): Unit = {
    OutWatch.render("#app", KBRouter())
  }

  object KBRouter extends Router {

    sealed trait Page

    object HomePage extends Page

    object AboutKBPage extends Page

    case class TaxonURL(id: String) extends Page

    case class EntityURL(id: String) extends Page

    case class GeneURL(id: String) extends Page

    case class GeneSimilarityURL(id: String) extends Page

    object FacetURL extends Page

    //FIXME add state for inferred presence/absence
    case class FacetURLP(params: String) extends Page with ParameterizedURL {

      def tab: FacetPage.FacetTab = param("tab").collect(FacetURLP.keyToTab).getOrElse(FacetPage.TaxaTab)

      def entity: Option[IRI] = param("entity").map(e => Vocab.expand(Curie(e)))

      def quality: Option[IRI] = param("quality").map(e => Vocab.expand(Curie(e)))

      def taxon: Option[IRI] = param("taxon").map(e => Vocab.expand(Curie(e)))

      def pub: Option[IRI] = param("pub").map(e => Vocab.expand(Curie(e)))

    }

    object FacetURLP {

      val tabToKey: Map[FacetPage.FacetTab, String] = Map(
        FacetPage.TaxaTab -> "taxa",
        FacetPage.PhenotypesTab -> "phenotypes",
        FacetPage.TaxonAnnotationsTab -> "taxonannotations",
        FacetPage.GenesTab -> "genes",
        FacetPage.GeneAnnotationsTab -> "geneannotations",
        FacetPage.PublicationsTab -> "publications")
      val keyToTab: Map[String, FacetPage.FacetTab] = tabToKey.map(_.swap)

      def urlForState(state: FacetPage.State): String = {
        val params = Map[String, Any]("tab" -> tabToKey(state.selectedTab))
          .add(state.selectedEntityPath.headOption.map(x => "entity" -> Vocab.compact(x).id))
          .add(state.selectedQualityPath.headOption.map(x => "quality" -> Vocab.compact(x).id))
          .add(state.selectedTaxonPath.headOption.map(x => "taxon" -> Vocab.compact(x).id))
          .add(state.selectedPublication.map(x => "pub" -> Vocab.compact(x).id))
        s"#/facet${toQueryQ(params)}"
      }

    }

    object OntotraceURL extends Page

    val baseUrl: BaseUrl = BaseUrl.until_# + "#"

    override def baseLayout(node: Observable[VNode]): VNode = BaseLayout.view(node)

    val config = RouterConfig { builder =>
      import builder._

      builder.rules(
        "/home".const(HomePage) ~> Home(),
        "/about/phenoscape/kb".const(AboutKBPage) ~> AboutKB(),
        ("/taxon" / string(".+")).caseClass[TaxonURL] ~> { case TaxonURL(id) => TaxonPage(TaxonPage.State(Vocab.expand(Curie(id)))) },
        ("/entity" / string(".+")).caseClass[EntityURL] ~> { case EntityURL(id) => EntityPage(EntityPage.State(Vocab.expand(Curie(id)))) },
        ("/gene" / string(".+")).caseClass[GeneURL] ~> { case GeneURL(id) => GenePage(GenePage.State(Vocab.expand(Curie(id)))) },
        ("/similarity/gene" / string(".+")).caseClass[GeneSimilarityURL] ~> { case GeneSimilarityURL(id) => GeneTaxonSimilarityPage(GeneTaxonSimilarityPage.State(Vocab.expand(Curie(id)), None)) },
        ("/facet?" ~ remainingPathOrBlank).caseClass[FacetURLP] ~> (params => FacetPage(FacetPage.State(params.tab, params.entity.toList, params.quality.toList, params.taxon.toList, params.pub, PhenotypicQuality, false, false, false))),
        "/facet".const(FacetURL) ~> FacetPage(FacetPage.State(FacetPage.TaxaTab, Nil, Nil, Nil, None, PhenotypicQuality, false, false, false)),
        "/ontotrace".const(OntotraceURL) ~> OntoTracePage(OntoTracePage.State(OntoTracePage.SimpleMode, None, None, false, false, Invalid(None), Invalid(None))))
        .notFound(Redirect(HomePage, replace = true))
    }
  }

  trait ParameterizedURL {

    def params: String

    private lazy val keyValues: Map[String, String] = params.split("&", -1).map { kv =>
      val parts = kv.split("=", 2).filter(_.nonEmpty)
      if (parts.size == 2) Some(parts(0) -> decodeURIComponent(parts(1)))
      else None
    }.filter(_.nonEmpty).flatten.toMap

    def param(key: String): Option[String] = keyValues.get(key)

  }

}