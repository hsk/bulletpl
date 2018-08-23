#include <QApplication>
#include <QWebView>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    QWebView view;
    view.setFixedSize(450,650);
    view.show();
    view.load(QUrl("http://localhost:3030/index.html"));

    return a.exec();
}

