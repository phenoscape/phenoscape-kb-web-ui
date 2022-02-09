package org.phenoscape.kb.ui

import com.raquo.laminar.api.L._
import org.phenoscape.kb.ui.App.{AboutPage, FacetPage, GeneTaxonSimilarityPage, OntoTracePage, Page, TaxonGeneSimilarityPage}
import org.phenoscape.kb.ui.components.MainSearch

object HomePageView {

  def view(updates: WriteBus[Page]): HtmlElement = {
    div(
      div(
        cls := "row",
        div(
          cls := "col-sm-12",
          p(
            """The Phenoscape Knowledgebase is a datastore of computable phenotypes for studies of evolution and genetics.
              You can explore a large number of manually curated annotations in the KB derived from the literature.
              These annotations, taken from trusted literature sources, report that a taxon (or genotype) shows variation in some quality (e.g. shape, size, etc.) in some entity (anatomical part),
              using controlled vocabulary terms from community ontologies that allow for semantic reasoning across diverse phenotypes from different organisms.""")
        )
      ),
      div(
        cls := "row",
        div(
          cls := "col-sm-8",
          div(
            cls := "row",
            div(
              cls := "col-sm-12",
              MainSearch.mainSearch(updates, "default")
            )
          ),
          div(
            cls := "row",
            div(
              cls := "col-sm-6",
              panel("Ontologies and Annotation")(
                h5(a(
                  role := "button",
                  onClick.mapTo(AboutPage) --> updates,
                  "Data sources")),
                p("Phenoscape builds on community-developed ontologies and public model organism databases."),
                h5(a(href := "https://wiki.phenoscape.org/wiki/Guide_to_Character_Annotation", "Guide to character annotation")),
                p("Phenoscape welcomes community contributions. Get started by reviewing our resources for data contributors.")
              )
            ),
            div(
              cls := "col-sm-6",
              panel("Software and Data")(
                h5(a(href := "https://rphenoscape.phenoscape.org", "RPhenoscape")),
                p("This package facilitates interfacing with the Phenoscape Knowledgebase for searching ontology terms, retrieving term info, and querying data matrices."),
                h5(a(href := "https://kb.phenoscape.org/api/v2-beta/docs/", "Web service API")),
                p("This website is built on our publicly accessible web service APIs. Service documentation and automatically generated code examples are available at the link above."),
                h5(a(href := "https://github.com/phenoscape/Phenex/wiki", "Phenex annotation software")),
                p("Phenex is an application for annotating character matrix files with ontology terms using the Entity–Quality syntax for describing phenotypes."),
                h5(a(href := "https://github.com/phenoscape", "Phenoscape code on GitHub")),
                p("Software developed by the Phenoscape project is freely available and open source.")
              )
            )
          )
        ),
        div(
          cls := "col-sm-4",
          panel("Analysis Tools", "primary")(
            h4(
              a(
                role := "button",
                onClick.mapTo(FacetPage()) --> updates,
                "Browse Data"
              )
            ),
            p("Browse phenotype annotations in the KB by anatomy and taxonomy."),
            h4(
              a(
                role := "button",
                onClick.mapTo(OntoTracePage) --> updates,
                "OntoTrace"
              )
            ),
            p("Export a synthetic character matrix of inferred knowledge about presence/absence of anatomical structures."),
            h4("Semantic Similarity"),
            p("Find ",
              a(
                role := "button",
                onClick.mapTo(GeneTaxonSimilarityPage()) --> updates,
                "taxonomic variation similar to the phenotypic profile of a gene"
              ),
              ", or ",
              a(
                role := "button",
                onClick.mapTo(TaxonGeneSimilarityPage()) --> updates,
                "developmental genetic phenotypes similar to variation within a taxon"),
              ".")
          )
        )
      )
    )
  }

  //FIXME remove outer div?
  def panel(heading: String, state: String = "default")(nodes: Modifier[Div]*): HtmlElement =
    div(
      div(
        cls := s"panel panel-$state",
        div(
          cls := "panel-heading",
          h5(
            cls := "example-heading",
            heading)),
        div(cls := "panel-body", nodes)
      ))

}
