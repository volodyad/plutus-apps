"use strict";

const HtmlWebpackPlugin = require("html-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const MonacoWebpackPlugin = require("monaco-editor-webpack-plugin");
const { webpackConfig } = require("purescript-web-common");
const path = require("path");

const pursSources = [
    'test/**/*.purs',
    'generated/**/*.purs',
    "web-common-plutus/src/**/*.purs",
    "web-common-playground/src/**/*.purs",
];

const dist = path.join(__dirname, "dist");

module.exports = webpackConfig(pursSources, (env) => {
    const isDevelopment = env === "development";
    return {
        devServer: {
            contentBase: dist,
            port: 8008,
            proxy: {
                "/api": {
                    target: "http://localhost:8080",
                },
            },
        },
        entry: "./entry.js",
        output: { path: dist },
        module: {
            rules: [
                {
                    test: /\.scss$/,
                    use: [MiniCssExtractPlugin.loader, "css-loader", "sass-loader"],
                },
            ],
        },
        resolve: {
            alias: {
                static: path.resolve(__dirname, "./static"),
                src: path.resolve(__dirname, "./src"),
            },
        },
        plugins: [
            new HtmlWebpackPlugin({
                template: `${process.env.WEB_COMMON_SRC}/static/index.html`,
                favicon: "static/favicon.ico",
                title: "Plutus Playground",
                productName: "plutus-playground",
                googleAnalyticsId: isDevelopment ? "UA-XXXXXXXXX-X" : "G-9FPZ01J8E4",
                segmentAnalyticsId: isDevelopment ? "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" : "0CEePM8LJUSpPoo2QGrXHDw4GKg4JFBo",
            }),
            new MonacoWebpackPlugin({
                languages: ["haskell"],
            }),
        ],
    };
});
