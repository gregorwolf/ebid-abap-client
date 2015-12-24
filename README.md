# EBID ABAP client

ABAP Client for the https://www.unternehmensverzeichnis.org/ REST API

## Prerequisites

To use this EBID ABAP client you have to register first at https://www.unternehmensverzeichnis.org/. Then use this user to create a  HTTP Connection to External Server in transation SM59. The connection should be named EBID. You have to use target host matching.unternehmensverzeichnis.org and Service No. 443. Under Security & Logon provide your unternehmensverzeichnis.org Username & Password from the registration. Switch SSL to Active and choose the SSL Certificate "SSL Client (Anonymous)". To estabish the SSL Conneciton you have to import the certificate path used to sign www.unternehmensverzeichnis.org to the "SSL Client (Anonymous)" entry in transaction STRUST.

## Installation

Install abapGit from https://github.com/larshp/abapGit and then simply clone this project by running the ZABAPGIT report.
