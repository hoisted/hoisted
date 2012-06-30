package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._

import org.hoisted._
import lib._

import net.liftmodules.FoBo

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    /*
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = 
	new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
			     Props.get("db.url") openOr 
			     "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
			     Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    // Use Lift's Mapper ORM to populate the database
    // you don't need to use Mapper to use Lift... use
    // any ORM you want
    Schemifier.schemify(true, Schemifier.infoF _, User)
    */

    // where to search snippet
    LiftRules.addToPackages("org.hoisted")
    
    FoBo.InitParam.JQuery=FoBo.JQuery171  
    FoBo.InitParam.ToolKit=FoBo.Bootstrap204
    FoBo.InitParam.ToolKit=FoBo.PrettifyJun2011
    FoBo.init()
    
    CMSStore.defaultHost

    Thread.sleep(500)

    LiftRules.dispatch.append(CMS)

    LiftRules.viewDispatch.append(CMSViewFinder)

    // Build SiteMap
    val entries = List(
      Menu.i("Home") / "index", // the simple way to declare a menu

      // more complex because this menu allows anything in the
      // /static path to be visible
      Menu(Loc("Static", Link(List("static"), true, "/static/index"), 
	       "Static Content"))) :::
    Nil

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMap(SiteMap(entries:_*))

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
    
    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    // LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    // Make a transaction span the whole HTTP request
  }
}
