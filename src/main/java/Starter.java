import de.saxsys.mvvmfx.FluentViewLoader;
import de.saxsys.mvvmfx.ViewTuple;
import javafx.application.Application;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import viewmodels.HomeViewModel;
import views.HomeView;

public class Starter extends Application{
    public static void main(String...args){
        Application.launch(args);
    }

    @Override
    public void start(Stage stage){
        stage.setTitle("Mecca Dauber Application");

        ViewTuple<HomeView, HomeViewModel> viewTuple = FluentViewLoader.fxmlView(HomeView.class).load();

        Parent root = viewTuple.getView();
        stage.setScene(new Scene(root));
        stage.show();
    }
}
