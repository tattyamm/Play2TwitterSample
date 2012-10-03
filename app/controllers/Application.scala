package controllers

import play.api._
import libs.oauth._
import libs.oauth.ConsumerKey
import libs.oauth.OAuth
import libs.oauth.RequestToken
import libs.oauth.ServiceInfo
import libs.ws.WS
import play.api.mvc._
import scala.Left
import scala.Right

object Application extends Controller {

  //twitterでアプリケーションを登録した時に取得する値
  //動かす時はここに入力してください。
  val consumerKey = ""
  val consumerSecret = ""

  /*
   * indexページ。ログイン、ログアウトのメニューと、twitter apiから情報を取得した結果を表示する。
   */
  def index = Action {
    request => {

      //セッションの中身の確認
      val accessToken = request.session.get("token").map {
        token =>
          println("token : " + token)
          token
      }.getOrElse {
        println("tokenが取得できなかった")
        ""
      }
      val accessTokenSecret = request.session.get("secret").map {
        secret =>
          println("secret : " + secret)
          secret
      }.getOrElse {
        println("secretが取得できなかった")
        ""
      }

      //ログイン判定
      // TODO こんな方法はダメなのでは。
      if (!(accessToken == "" && accessTokenSecret == "")) {
        println("ログイン中")
      } else {
        println("ログアウト中")
      }

      //認証情報の作成
      val oauthCalculator = OAuthCalculator(ConsumerKey(consumerKey, consumerSecret), RequestToken(accessToken, accessTokenSecret))

      //タイムラインの取得
      val url = "https://api.twitter.com/1.1/statuses/home_timeline.json"
      Async {
        WS.url(url).sign(oauthCalculator).get().map {
          response =>
            Ok(views.html.index(response.body))
        }
      }

    }

  }

  /*
   * セッションを破棄してindexページにリダイレクト
   */
  def logout = Action {
    Redirect(routes.Application.index).withNewSession
  }

  /*
   * twitter認証系のテスト
   * ここからほぼコピーしています　https://github.com/playframework-ja/Play20/wiki/ScalaOAuth
   */
  val KEY = ConsumerKey(consumerKey, consumerSecret)

  val TWITTER = OAuth(ServiceInfo(
    "https://api.twitter.com/oauth/request_token",
    "https://api.twitter.com/oauth/access_token",
    "https://api.twitter.com/oauth/authorize", KEY),
    false)

  def authenticate = Action {
    request =>
      request.queryString.get("oauth_verifier").flatMap(_.headOption).map {
        verifier =>
          val tokenPair = sessionTokenPair(request).get
          // We got the verifier; now get the access token, store it and back to index
          println("認証されました。アクセストークンを取得し、保存し、indexに戻ります")
          TWITTER.retrieveAccessToken(tokenPair, verifier) match {
            case Right(t) => {
              // We received the authorized tokens in the OAuth object - store it before we proceed
              println("Oauthオブジェクトからアクセストークンを受け取りました。それを保存します。")
              Redirect(routes.Application.index).withSession("token" -> t.token, "secret" -> t.secret)
            }
            case Left(e) => throw e
          }
      }.getOrElse(
        TWITTER.retrieveRequestToken("http://localhost:9000/auth") match {
          //コールバックURL
          case Right(t) => {
            // We received the unauthorized tokens in the OAuth object - store it before we proceed
            println("認証されてないトークンを受け取りました。それを保存します。")
            Redirect(TWITTER.redirectUrl(t.token)).withSession("token" -> t.token, "secret" -> t.secret)
          }
          case Left(e) => throw e
        })
  }

  def sessionTokenPair(implicit request: RequestHeader): Option[RequestToken] = {
    for {
      token <- request.session.get("token")
      secret <- request.session.get("secret")
    } yield {
      RequestToken(token, secret)
    }
  }


}